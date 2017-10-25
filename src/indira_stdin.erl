%%%---------------------------------------------------------------------------
%%% @doc
%%%   STDIN reader with backpressure.
%%%
%%%   The module needs Erlang VM to be started with `-noinput' option, so
%%%   STDIN descriptor is not used by the runtime.
%%%
%%%   The module mimics the behaviour of {@link gen_tcp}.
%%%
%%%   Active handle sends:
%%%   <ul>
%%%     <li>{@type @{stdin, handle(), Data :: binary()@}}</li>
%%%     <li>{@type @{stdin_closed, handle()@}}</li>
%%%     <li>{@type @{stdin_error, handle(), Reason :: file:posix()@}}</li>
%%%   </ul>
%%%
%%%   This module can be used before Indira application starts.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_stdin).

-export([open/1, open/2, close/1]).
-export([recv/2, recv/3]).
-export([controlling_process/2, setopts/2, getopts/2]).
-export([format_error/1]).

-export_type([option/0, option_name/0]).
-export_type([handle/0]).
-export_type([message/0]).

%%%---------------------------------------------------------------------------

-define(DRIVER_NAME, "indira_stdin_drv").

-define(PORT_COMMAND_RECV,          4).
-define(PORT_COMMAND_RECV_CANCEL,   5).
-define(PORT_COMMAND_SETOPTS,       6).
-define(PORT_COMMAND_GETOPTS,       7).

%%%---------------------------------------------------------------------------

-type handle() :: port().
%% File descriptor port handle.

-type message() :: {stdin, Handle :: handle(), Data :: binary() | string()}
                 | {stdin_closed, Handle :: handle()}
                 | {stdin_error, Handle :: handle(), Reason :: term()}.
%% Messages sent in `{active,true}' and `{active,once}' modes.
%%
%% <ul>
%%   <li>`stdin' -- regular payload</li>
%%   <li>`stdin_error' -- read error, after which read descriptor is closed;
%%       `Reason' can be translated to a usable message with
%%       {@link format_error/1} function</li>
%%   <li>`stdin_closed' -- EOF on the handle (the handle is closed)</li>
%% </ul>
%%
%% Most notable errors:
%% <ul>
%%   <li>`{stdin_error,Port,closed}' -- last message before EOF was
%%       incomplete</li>
%%   <li>`{stdin_error,Port,emsgsize}' -- message was larger than `packet_size'
%%       {@type option()}</li>
%% </ul>

-type option() :: list | binary | {mode, list | binary}
                | {active, true | false | once}
                | {packet, 0 | 1 | 2 | 4 | raw | line}
                | {packet_size, pos_integer()}.
%% Options controlling how data from handle is read.
%%
%% <ul>
%%   <li>`list', `binary', `{mode,_}' -- data is returned as {@type string()}
%%       or as {@link binary()} (for {@type @{packet, raw | 1 | 2 | 4@}},
%%       `binary' mode should be a little faster)</li>
%%   <li>`{active,_}' -- set receiving {@type message()} messages from port;
%%       when set to `false', the port will not send any data on its own and
%%       {@link recv/2} or {@link recv/3} call is required</li>
%%   <li>`{packet,_}' -- packet format to read from the port; `{packet, 0}'
%%       and `{packet, raw}' are synonymous, with the latter being the
%%       canonical form ({@link getopts/2})</li>
%%   <li>`{packet_size,_}' -- maximum allowed size of a single packet; if
%%       a packet read exceeds this size, read returns an error and
%%       handle is closed; maximum supported size is 67108864 (64 MB)</li>
%% </ul>

-type option_name() :: mode | active | packet | packet_size.

%%%---------------------------------------------------------------------------
%%% opening and closing
%%%---------------------------------------------------------------------------

%% @doc Open a reading handle for STDIN (descriptor 0).
%%
%%   Default options for the STDIN handle are: `{active,false}',
%%   `{mode,list}', `{packet,line}', `{packet_size,65536}'.

-spec open([option()]) ->
  {ok, handle()} | {error, badarg | system_limit | file:posix()}.

open(Options) ->
  open(0, Options).

%% @doc Open a reading handle for specific file descriptor.
%%
%%   Default options for the handle are: `{active,false}',
%%   `{mode,list}', `{packet,line}', `{packet_size,65536}'.

-spec open(non_neg_integer(), [option()]) ->
  {ok, handle()} | {error, badarg | system_limit | file:posix()}.

open(FD, Options) when is_integer(FD), FD >= 0, FD =< 16#ffffffff ->
  DefaultOptions = [
    {mode, list},
    {active, false},
    {packet, line},
    {packet_size, 65536}
  ],
  case encode_options(Options ++ DefaultOptions) of
    {ok, OptData} ->
      ok = load_port_driver(),
      Command = ?DRIVER_NAME ++ " " ++ integer_to_list(FD),
      try open_port({spawn_driver, Command}, [binary]) of
        Handle ->
          case port_control(Handle, ?PORT_COMMAND_SETOPTS, OptData) of
            <<>> ->
              {ok, Handle};
            Error ->
              port_close(Handle),
              Reason = binary_to_atom(Error, latin1),
              {error, Reason}
          end
      catch
        error:Reason ->
          {error, Reason}
      after
        % the driver will be unloaded only after the port terminates, and this
        % avoids problems with counting load/unload when ownership is
        % transferred
        unload_port_driver()
      end;
    {error, badarg} ->
      {error, badarg}
  end.

%% @doc Close the handle.
%%
%%   Underlying file descriptor is closed as well, unless it's 0, 1, or 2.

-spec close(handle()) ->
  ok.

close(Handle) ->
  try
    unlink(Handle),
    port_close(Handle)
  catch
    % this could be caused by port already being closed, which is expected in
    % some cases
    error:badarg -> ignore
  end,
  ok.

%%%---------------------------------------------------------------------------
%%% handle operations
%%%---------------------------------------------------------------------------

%% @doc Read data from the handle.
%%
%%   Special errors:
%%   <ul>
%%     <li>`closed' -- reading descriptor closed</li>
%%     <li>`eintr' -- handle mode changed to active while `recv()'</li>
%%     <li>`ealready' -- another `recv()' call already in progress</li>
%%     <li>`einval' -- handle is in active mode</li>
%%   </ul>

-spec recv(handle(), non_neg_integer()) ->
  {ok, binary()} | eof | {error, closed | file:posix()}.

recv(Handle, Length) ->
  recv(Handle, Length, infinity).

%% @doc Read data from the handle.
%%
%%   Notable errors:
%%   <ul>
%%     <li>`timeout' -- nothing to read</li>
%%     <li>`closed' -- reading descriptor closed</li>
%%     <li>`eintr' -- handle mode changed to active while `recv()'</li>
%%     <li>`ealready' -- another `recv()' call already in progress</li>
%%     <li>`einval' -- handle is in active mode</li>
%%   </ul>

-spec recv(handle(), non_neg_integer(), timeout()) ->
    {ok, string() | binary()}
  | eof
  | {error, timeout | closed | badarg | file:posix()}.

recv(Handle, Length, infinity = _Timeout)
when is_integer(Length), Length >= 0 ->
  case control_port(Handle, ?PORT_COMMAND_RECV, <<Length:32>>) of
    ok ->
      reply_wait(Handle, 100);
    {error, Reason} ->
      {error, Reason}
  end;
recv(Handle, Length, Timeout)
when is_integer(Length), Length >= 0, is_integer(Timeout), Timeout >= 0 ->
case control_port(Handle, ?PORT_COMMAND_RECV, <<Length:32>>) of
    ok ->
      receive
        {stdin_reply, Handle, {ok, Data}} -> {ok, Data};
        {stdin_reply, Handle, eof} -> eof;
        {stdin_reply, Handle, {error, Reason}} -> {error, Reason}
      after Timeout ->
        control_port(Handle, ?PORT_COMMAND_RECV_CANCEL, <<>>),
        receive
          {stdin_reply, Handle, {ok, Data}} -> {ok, Data};
          {stdin_reply, Handle, eof} -> eof;
          {stdin_reply, Handle, {error, Reason}} -> {error, Reason}
        after 0 ->
          {error, timeout}
        end
      end;
    {error, Reason} ->
      {error, Reason}
  end;
recv(_Handle, _Length, _Timeout) ->
  {error, badarg}.

%% @doc Set the handle owner.

-spec controlling_process(handle(), pid()) ->
  ok | {error, not_owner | closed | badarg}.

controlling_process(Handle, Pid) ->
  try erlang:port_info(Handle, connected) of
    {connected, Pid} ->
      ok; % already the owner
    {connected, Owner} when Owner /= self() ->
      {error, not_owner};
    {connected, _OldOwner} ->
      try
        port_connect(Handle, Pid),
        unlink(Handle),
        ok
      catch
        _:_ ->
          {error, closed}
      end;
    undefined ->
      {error, closed}
  catch
    _:_ ->
      {error, badarg}
  end.

%% @doc Set one or more handle options.

-spec setopts(handle(), [option()]) ->
  ok | {error, badarg}.

setopts(Handle, [{active, true}] = _Options) ->
  control_port(Handle, ?PORT_COMMAND_SETOPTS, <<16#0010:32, 0:32>>);
setopts(Handle, [{active, false}] = _Options) ->
  control_port(Handle, ?PORT_COMMAND_SETOPTS, <<16#0020:32, 0:32>>);
setopts(Handle, [{active, once}] = _Options) ->
  control_port(Handle, ?PORT_COMMAND_SETOPTS, <<16#0030:32, 0:32>>);
setopts(Handle, Options) ->
  case encode_options(Options) of
    {ok, OptData} -> control_port(Handle, ?PORT_COMMAND_SETOPTS, OptData);
    {error, badarg} -> {error, badarg}
  end.

%% @doc Get one or more handle options.

-spec getopts(handle(), [option_name()]) ->
  {ok, [option()]} | {error, badarg}.

getopts(Handle, Options) ->
  try port_control(Handle, ?PORT_COMMAND_GETOPTS, <<>>) of
    <<Flags:32, Size:32>> -> decode_options(Options, Flags, Size)
  catch
    _:_ -> {error, badarg}
  end.

%%%---------------------------------------------------------------------------
%%% describing errors
%%%---------------------------------------------------------------------------

%% @doc Convert a `Reason' from error tuple into usable error message.

-spec format_error(term()) ->
  string().

format_error(badarg    = _Reason) -> "invalid argument";
format_error(timeout   = _Reason) -> "operation timed out";
format_error(closed    = _Reason) -> "connection is closed";
format_error(not_owner = _Reason) -> "not the owner of the handle";
format_error(Reason) -> file:format_error(Reason).

%%%---------------------------------------------------------------------------
%%% private helpers
%%%---------------------------------------------------------------------------

%% @doc Load port driver.

load_port_driver() ->
  case code:lib_dir(indira, priv) of
    {error, bad_name} ->
      erlang:error({missing_application, indira});
    PrivDir ->
      case erl_ddll:load(PrivDir, ?DRIVER_NAME) of
        ok -> ok;
        {error, Reason} -> erlang:error(Reason)
      end
  end.

%% @doc Unload port driver.

unload_port_driver() ->
  erl_ddll:unload(?DRIVER_NAME).

%% @doc Send to a handle a command that returns a {@type file:posix()} atom
%%   name on error.

-spec control_port(handle(), integer(), iolist() | binary()) ->
  ok | {error, badarg | file:posix()}.

control_port(Handle, Command, Data) ->
  try port_control(Handle, Command, Data) of
    <<>> ->
      ok;
    Error ->
      Reason = binary_to_atom(Error, latin1),
      {error, Reason}
  catch
    _:_ ->
      {error, badarg}
  end.

%% @doc Encode option list as a binary ready to be sent as port control
%%   command.

-spec encode_options([option()]) ->
  {ok, binary()} | {error, badarg}.

encode_options(Options) ->
  try lists:foldr(fun set_option/2, {0, 0}, Options) of
    {Flags, Size} -> {ok, <<Flags:32, Size:32>>}
  catch
    _:_ -> {error, badarg}
  end.

%%----------------------------------------------------------
%% set_option() {{{

%% @doc {@link lists:foldr/3} workhorse for {@link encode_options/1}.

-spec set_option(option(), {non_neg_integer(), non_neg_integer()}) ->
  {non_neg_integer(), non_neg_integer()}.

set_option(list,            {F, S}) -> {(F band 16#fff0) bor 16#0001, S};
set_option({mode, list},    {F, S}) -> {(F band 16#fff0) bor 16#0001, S};
set_option(binary,          {F, S}) -> {(F band 16#fff0) bor 16#0002, S};
set_option({mode, binary},  {F, S}) -> {(F band 16#fff0) bor 16#0002, S};

set_option({active, true},  {F, S}) -> {(F band 16#ff0f) bor 16#0010, S};
set_option({active, false}, {F, S}) -> {(F band 16#ff0f) bor 16#0020, S};
set_option({active, once},  {F, S}) -> {(F band 16#ff0f) bor 16#0030, S};

set_option({packet, raw},   {F, S}) -> {(F band 16#f0ff) bor 16#0100, S};
set_option({packet, 0},     {F, S}) -> {(F band 16#f0ff) bor 16#0100, S};
set_option({packet, 1},     {F, S}) -> {(F band 16#f0ff) bor 16#0200, S};
set_option({packet, 2},     {F, S}) -> {(F band 16#f0ff) bor 16#0300, S};
set_option({packet, 4},     {F, S}) -> {(F band 16#f0ff) bor 16#0400, S};
set_option({packet, line},  {F, S}) -> {(F band 16#f0ff) bor 16#0500, S};

set_option({packet_size, Size}, {Flags, _OldSize})
when is_integer(Size), Size > 0 ->
  {Flags, Size}.

%% }}}
%%----------------------------------------------------------

%% @doc Fill requested options with data read from port.

-spec decode_options([option_name()], non_neg_integer(), non_neg_integer()) ->
  {ok, [option()]} | {error, badarg}.

decode_options(OptionNames, Flags, Size) ->
  try lists:foldr(fun get_option/2, {[], Flags, Size}, OptionNames) of
    {Options, _, _} -> {ok, Options}
  catch
    _:_ -> {error, badarg}
  end.

%%----------------------------------------------------------
%% get_option() {{{

%% @doc {@link lists:foldr/3} workhorse for {@link decode_options/3}.

-spec get_option(option_name(),
                 {[option()], non_neg_integer(), non_neg_integer()}) ->
  {[option()], non_neg_integer(), non_neg_integer()}.

get_option(mode = _Name, {Opts, Flags, Size} = _Acc) ->
  case Flags band 16#fff0 of
    16#0001 -> {[{mode, list} | Opts], Flags, Size};
    16#0002 -> {[{mode, binary} | Opts], Flags, Size}
  end;
get_option(active = _Name, {Opts, Flags, Size} = _Acc) ->
  case Flags band 16#ff0f of
    16#0010 -> {[{active, true}  | Opts], Flags, Size};
    16#0020 -> {[{active, false} | Opts], Flags, Size};
    16#0030 -> {[{active, once}  | Opts], Flags, Size}
  end;
get_option(packet = _Name, {Opts, Flags, Size} = _Acc) ->
  case Flags band 16#f0ff of
    16#0100 -> {[{packet, raw}  | Opts], Flags, Size};
    16#0200 -> {[{packet, 1}    | Opts], Flags, Size};
    16#0300 -> {[{packet, 2}    | Opts], Flags, Size};
    16#0400 -> {[{packet, 4}    | Opts], Flags, Size};
    16#0500 -> {[{packet, line} | Opts], Flags, Size}
  end;
get_option(packet_size = _Name, {Opts, Flags, Size} = _Acc) ->
  {[{packet_size, Size} | Opts], Flags, Size}.

%% }}}
%%----------------------------------------------------------

%% @doc Handle monitor routine that waits for the handle to reply.
%%
%%   If the ports dies silently before sending an ACK/NAK, `{error,closed}'
%%   is returned.

-spec reply_wait(handle(), timeout()) ->
  Reply :: term() | {error, closed}.

reply_wait(Handle, Interval) ->
  receive
    {stdin_reply, Handle, Reply} -> Reply
  after Interval ->
    case erlang:port_info(Handle, connected) of
      {connected, _} -> reply_wait(Handle, Interval); % still alive
      undefined -> {error, closed}
    end
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
