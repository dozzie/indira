%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   AF_UNIX sockets.
%%%
%%%   The module mimics {@link gen_tcp} behaviour.
%%%
%%%   Active socket sends:
%%%   <ul>
%%%     <li>{@type @{unix, socket(), Data :: binary()@}}</li>
%%%     <li>{@type @{unix_closed, socket()@}}</li>
%%%     <li>{@type @{unix_error, socket(), Reason :: inet:posix()@}}</li>
%%%   </ul>
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_af_unix).

%% server socket API
-export([listen/2, accept/1, accept/2]).
-export([chmod/2, chown/2, chgrp/2, stat/1]).
%% connection socket API
-export([connect/2, send/2, recv/2, recv/3]).
%% common to server and connection sockets
-export([controlling_process/2, setopts/2, getopts/2]).
-export([close/1]).
-export([format_error/1]).

%% private exports for indira_unix
-export([load_port_driver/0, unload_port_driver/0]).

-export_type([address/0, option/0, option_name/0]).
-export_type([socket/0, server_socket/0, connection_socket/0]).
-export_type([device/0, inode/0]).
-export_type([message/0]).

%%%---------------------------------------------------------------------------

-define(DRIVER_NAME, "indira_af_unix_drv").

-define(PORT_COMMAND_INIT_LISTEN,   0).
-define(PORT_COMMAND_INIT_CONNECT,  1).
-define(PORT_COMMAND_ACCEPT,        2).
-define(PORT_COMMAND_ACCEPT_CANCEL, 3).
-define(PORT_COMMAND_RECV,          4).
-define(PORT_COMMAND_RECV_CANCEL,   5).
-define(PORT_COMMAND_SETOPTS,       6).
-define(PORT_COMMAND_GETOPTS,       7).
-define(PORT_COMMAND_CHMOD,         8).
-define(PORT_COMMAND_CHOWN_USER,    9).
-define(PORT_COMMAND_CHOWN_UID,    10).
-define(PORT_COMMAND_CHGRP_GROUP,  11).
-define(PORT_COMMAND_CHGRP_GID,    12).
-define(PORT_COMMAND_STAT,         13).

%%%---------------------------------------------------------------------------

-type address() :: file:filename().
%% Socket address.

-type socket() :: port().
%% Socket handle.

-type connection_socket() :: socket().
%% Client socket handle (either from connecting to socket or from accepting
%% a connection on a server socket).

-type server_socket() :: socket().
%% Server (listening) socket handle.

-type message() :: {unix, Socket :: connection_socket(),
                     Data :: binary() | string()}
                 | {unix_closed, Socket :: connection_socket()}
                 | {unix_error, Socket :: connection_socket(),
                     Reason :: term()}.
%% Messages sent in `{active,true}' and `{active,once}' modes.
%%
%% <ul>
%%   <li>`unix' -- regular payload</li>
%%   <li>`unix_error' -- read error, after which read descriptor is closed;
%%       `Reason' can be translated to a usable message with
%%       {@link format_error/1} function</li>
%%   <li>`unix_closed' -- EOF on the socket (the socket is closed)</li>
%% </ul>
%%
%% Most notable errors:
%% <ul>
%%   <li>`{unix_error,Port,closed}' -- last message before EOF was
%%       incomplete</li>
%%   <li>`{unix_error,Port,emsgsize}' -- message was larger than `packet_size'
%%       {@type option()}</li>
%% </ul>

-type option() :: list | binary | {mode, list | binary}
                | {active, true | false | once}
                | {packet, 0 | 1 | 2 | 4 | raw | line}
                | {packet_size, pos_integer()}.
%% Options controlling how data from socket is read.
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
%%       socket is closed; maximum supported size is 67108864 (64 MB)</li>
%% </ul>

-type option_name() :: mode | active | packet | packet_size.

-type device() :: non_neg_integer().
-type inode() :: non_neg_integer().

%%%---------------------------------------------------------------------------
%%% server socket API
%%%---------------------------------------------------------------------------

%% @doc Setup a socket listening on specified address.

-spec listen(address(), [option()]) ->
  {ok, server_socket()} | {error, badarg | system_limit | inet:posix()}.

listen(Address, Options) when is_list(Address); is_binary(Address) ->
  DefaultOptions = [
    {mode, list},
    {active, false},
    {packet, raw},
    {packet_size, 16384}
  ],
  case encode_options(Options ++ DefaultOptions) of
    {ok, OptData} ->
      case spawn_port(?PORT_COMMAND_INIT_LISTEN, Address) of
        {ok, Socket} ->
          ok = control_port(Socket, ?PORT_COMMAND_SETOPTS, OptData),
          {ok, Socket};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, badarg} ->
      {error, badarg}
  end.

%% @doc Accept a client connection.
%%   The function waits infinitely for a client.

-spec accept(server_socket()) ->
  {ok, connection_socket()} | {error, closed | system_limit | badarg | inet:posix()}.

accept(Socket) ->
  accept(Socket, infinity).

%% @doc Accept a client connection.

-spec accept(server_socket(), timeout()) ->
  {ok, connection_socket()} | {error, timeout | closed | system_limit | badarg | inet:posix()}.

accept(Socket, infinity = _Timeout) ->
  case control_port(Socket, ?PORT_COMMAND_ACCEPT, <<>>) of
    ok ->
      case reply_wait(Socket, 100) of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end;
accept(Socket, Timeout) when is_integer(Timeout), Timeout >= 0 ->
  case control_port(Socket, ?PORT_COMMAND_ACCEPT, <<>>) of
    ok ->
      receive
        {unix_reply, Socket, {ok, Conn}} -> {ok, Conn};
        {unix_reply, Socket, {error, Reason}} -> {error, Reason}
      after Timeout ->
        control_port(Socket, ?PORT_COMMAND_ACCEPT_CANCEL, <<>>),
        receive
          {unix_reply, Socket, {ok, Conn}} -> {ok, Conn};
          {unix_reply, Socket, {error, Reason}} -> {error, Reason}
        after 0 ->
          {error, timeout}
        end
      end;
    {error, Reason} ->
      {error, Reason}
  end;
accept(_Socket, _Timeout) ->
  {error, badarg}.

%% @doc Change permissions of a listening socket.

-spec chmod(server_socket(), non_neg_integer()) ->
  ok | {error, badarg | inet:posix()}.

chmod(Socket, Mode) when is_integer(Mode), Mode >= 0, Mode =< 8#7777 ->
  control_port(Socket, ?PORT_COMMAND_CHMOD, <<Mode:16>>);
chmod(_Socket, _Mode) ->
  {error, badarg}.

%% @doc Change owner of a listening socket.

-spec chown(server_socket(), string() | binary() | non_neg_integer()) ->
  ok | {error, badarg | nxuser | inet:posix()}.

chown(Socket, User) when is_list(User); is_binary(User) ->
  control_port(Socket, ?PORT_COMMAND_CHOWN_USER, User);
chown(Socket, User) when is_integer(User), User >= 0, User =< 16#ffffffff ->
  control_port(Socket, ?PORT_COMMAND_CHOWN_UID, <<User:32>>);
chown(_Socket, _User) ->
  {error, badarg}.

%% @doc Change group of a listening socket.

-spec chgrp(server_socket(), string() | binary() | non_neg_integer()) ->
  ok | {error, badarg | nxgroup | inet:posix()}.

chgrp(Socket, Group) when is_list(Group); is_binary(Group) ->
  control_port(Socket, ?PORT_COMMAND_CHGRP_GROUP, Group);
chgrp(Socket, Group) when is_integer(Group), Group >= 0, Group =< 16#ffffffff ->
  control_port(Socket, ?PORT_COMMAND_CHGRP_GID, <<Group:32>>);
chgrp(_Socket, _Group) ->
  {error, badarg}.

%% @doc Retrieve device and inode number where the listening socket is placed.

-spec stat(server_socket()) ->
  {ok, {device(), inode()}} | {error, badarg}.

stat(Socket) ->
  try port_control(Socket, ?PORT_COMMAND_STAT, <<>>) of
    <<Dev:64, Inode:64>> -> {ok, {Dev, Inode}}
  catch
    _:_ -> {error, badarg}
  end.

%%%---------------------------------------------------------------------------
%%% connection socket
%%%---------------------------------------------------------------------------

%% @doc Connect to specified socket.
%%   Resulting connection defaults to passive (if not specified in options).
%%
%%   This function allows to send to datagram sockets as well, but
%%   {@link recv/2} and {@link recv/3} won't work on such socket.

-spec connect(address(), [option()]) ->
  {ok, connection_socket()} | {error, badarg | system_limit | inet:posix()}.

connect(Address, Options) when is_list(Address); is_binary(Address) ->
  case encode_options(Options) of
    {ok, OptData} ->
      case spawn_port(?PORT_COMMAND_INIT_CONNECT, Address) of
        {ok, Socket} ->
          ok = control_port(Socket, ?PORT_COMMAND_SETOPTS, OptData),
          {ok, Socket};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, badarg} ->
      {error, badarg}
  end.

%% @doc Send data to the socket.

-spec send(connection_socket(), iolist()) ->
  ok | {error, closed | badarg | inet:posix()}.

send(Socket, Data) ->
  try port_command(Socket, Data) of
    _ -> reply_wait(Socket, 100)
  catch
    error:badarg ->
      {error, badarg}
  end.

%% @doc Read data from the socket.
%%
%%   Special errors:
%%   <ul>
%%     <li>`closed' -- reading descriptor closed</li>
%%     <li>`eintr' -- socket mode changed to active while `recv()'</li>
%%     <li>`ealready' -- another `recv()' call already in progress</li>
%%     <li>`einval' -- socket is in active mode</li>
%%   </ul>

-spec recv(connection_socket(), non_neg_integer()) ->
  {ok, binary()} | eof | {error, closed | inet:posix()}.

recv(Socket, Length) ->
  recv(Socket, Length, infinity).

%% @doc Read data from the socket.
%%
%%   Notable errors:
%%   <ul>
%%     <li>`timeout' -- nothing to read</li>
%%     <li>`closed' -- reading descriptor closed</li>
%%     <li>`eintr' -- socket mode changed to active while `recv()'</li>
%%     <li>`ealready' -- another `recv()' call already in progress</li>
%%     <li>`einval' -- socket is in active mode</li>
%%   </ul>

-spec recv(connection_socket(), non_neg_integer(), timeout()) ->
    {ok, string() | binary()}
  | eof
  | {error, timeout | closed | badarg | inet:posix()}.

recv(Socket, Length, infinity = _Timeout)
when is_integer(Length), Length >= 0 ->
  case control_port(Socket, ?PORT_COMMAND_RECV, <<Length:32>>) of
    ok ->
      reply_wait(Socket, 100);
    {error, Reason} ->
      {error, Reason}
  end;
recv(Socket, Length, Timeout)
when is_integer(Length), Length >= 0, is_integer(Timeout), Timeout >= 0 ->
case control_port(Socket, ?PORT_COMMAND_RECV, <<Length:32>>) of
    ok ->
      receive
        {unix_reply, Socket, {ok, Data}} -> {ok, Data};
        {unix_reply, Socket, eof} -> eof;
        {unix_reply, Socket, {error, Reason}} -> {error, Reason}
      after Timeout ->
        control_port(Socket, ?PORT_COMMAND_RECV_CANCEL, <<>>),
        receive
          {unix_reply, Socket, {ok, Data}} -> {ok, Data};
          {unix_reply, Socket, eof} -> eof;
          {unix_reply, Socket, {error, Reason}} -> {error, Reason}
        after 0 ->
          {error, timeout}
        end
      end;
    {error, Reason} ->
      {error, Reason}
  end;
recv(_Socket, _Length, _Timeout) ->
  {error, badarg}.

%% @doc Set the socket owner.

-spec controlling_process(socket(), pid()) ->
  ok | {error, not_owner | closed | badarg}.

controlling_process(Socket, Pid) ->
  try erlang:port_info(Socket, connected) of
    {connected, Pid} ->
      ok; % already the owner
    {connected, Owner} when Owner /= self() ->
      {error, not_owner};
    {connected, _OldOwner} ->
      try
        port_connect(Socket, Pid),
        unlink(Socket),
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

%% @doc Set one or more socket options.

-spec setopts(socket(), [option()]) ->
  ok | {error, badarg}.

setopts(Socket, Options) ->
  case encode_options(Options) of
    {ok, OptData} -> control_port(Socket, ?PORT_COMMAND_SETOPTS, OptData);
    {error, badarg} -> {error, badarg}
  end.

%% @doc Get one or more socket options.

-spec getopts(socket(), [option_name()]) ->
  {ok, [option()]} | {error, badarg}.

getopts(Socket, Options) ->
  try port_control(Socket, ?PORT_COMMAND_GETOPTS, <<>>) of
    <<Flags:32, Size:32>> -> decode_options(Options, Flags, Size)
  catch
    _:_ -> {error, badarg}
  end.

%%%---------------------------------------------------------------------------
%%% common to server and connection
%%%---------------------------------------------------------------------------

%% @doc Close server connection.

-spec close(socket()) ->
  ok.

close(Socket) ->
  try
    unlink(Socket),
    port_close(Socket)
  catch
    % this could be caused by port already being closed, which is expected in
    % some cases
    error:badarg -> ignore
  end,
  ok.

%% @doc Convert a `Reason' from error tuple into usable error message.

-spec format_error(term()) ->
  string().

format_error(badarg    = _Reason) -> "invalid argument";
format_error(timeout   = _Reason) -> "operation timed out";
format_error(closed    = _Reason) -> "connection is closed";
format_error(not_owner = _Reason) -> "not the owner of the socket";
format_error(Reason) -> inet:format_error(Reason).

%%%---------------------------------------------------------------------------
%%% private exports
%%%---------------------------------------------------------------------------

%% @private
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

%% @private
%% @doc Unload port driver.

unload_port_driver() ->
  erl_ddll:unload(?DRIVER_NAME).

%%%---------------------------------------------------------------------------
%%% private helpers
%%%---------------------------------------------------------------------------

%% @doc Spawn a socket port and send it an initialization command.

-spec spawn_port(integer(), iolist() | binary()) ->
  {ok, socket()} | {error, badarg | system_limit | inet:posix()}.

spawn_port(Command, Data) ->
  try open_port({spawn_driver, ?DRIVER_NAME}, [binary]) of
    Socket ->
      case port_control(Socket, Command, Data) of
        <<>> ->
          {ok, Socket};
        Error ->
          port_close(Socket),
          Reason = binary_to_atom(Error, latin1),
          {error, Reason}
      end
  catch
    error:Reason ->
      {error, Reason}
  end.

%% @doc Send to a socket a command that returns a {@type inet:posix()} atom
%%   name on error.

-spec control_port(socket(), integer(), iolist() | binary()) ->
  ok | {error, badarg | inet:posix()}.

control_port(Socket, Command, Data) ->
  try port_control(Socket, Command, Data) of
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

%% @doc Socket monitor routine that waits for the socket to reply.
%%
%%   If the ports dies silently before sending an ACK/NAK, `{error,closed}'
%%   is returned.

-spec reply_wait(socket(), timeout()) ->
  Reply :: term() | {error, closed}.

reply_wait(Socket, Interval) ->
  receive
    {unix_reply, Socket, Reply} -> Reply
  after Interval ->
    case erlang:port_info(Socket, connected) of
      {connected, _} -> reply_wait(Socket, Interval); % still alive
      undefined -> {error, closed}
    end
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
