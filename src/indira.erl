%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira interface for `escript' scripts.
%%%
%%%   This module contains functions to be called from `escript' script and
%%%   utility functions for {@link gen_indira_listener. listeners}. It also
%%%   documents how to configure Indira ({@section Indira configuration}).
%%%   Protocol used to issue commands is described in {@link
%%%   gen_indira_listener} module.
%%%
%%%   Indira is intended to be started as the first application in `escript'.
%%%
%%%   == Indira configuration ==
%%%
%%%   Indira uses several configuration keys ({@link application:get_env/2}),
%%%   of which two are most important:
%%%   <i>listen</i> and <i>command</i>.
%%%
%%%   The value of <i>command</i> is a module name (with an arbitrary
%%%   parameter) that implements {@link gen_indira_command} behaviour. It can
%%%   also be a function, but this way is not recommended. See {@link
%%%   gen_indira_command} for details.
%%%
%%%   The value of <i>listen</i> is a list of pairs `{Mod,Args}'. `Mod' is
%%%   a name (atom) of module which is considered to be listener's entry
%%%   point. `Args' is an argument passed to `Mod:child_spec/2'. All listeners
%%%   must start successfully (this may became configurable in the future).
%%%   For more details see {@link gen_indira_listener}.
%%%
%%%   A file passed to <i>-config</i> VM option could look like this:
%```
%[
%  {indira, [
%    {listen, [
%      {indira_tcp, {"localhost", 16667}},
%      {indira_unix, "/var/run/my_app.sock"}
%    ]},
%    {command, {my_app_command, []}}
%  ]},
%  % ...
%].
%'''
%%%
%%% @TODO Notice if no listeners defined (see {@link
%%%   indira_listener_sup:init/1}).
%%% @TODO Channel types defined by operator (to differentiate between unix
%%%   socket, SSL and TCP).
%%% @TODO Add some channel-defined context to commands (e.g. for operation
%%%   logging).
%%% @TODO Don't crash BEAM when Indira failed to start. Rather, provide means
%%%   to detect it in startup script.
%%%
%%% @see gen_indira_command
%%% @see indira_tcp
%%% @see indira_udp
%%% @see indira_unix
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(indira).

%% API for running from command line (`erl -s indira')
-export([start/0]).

%% API for escript
-export([set_option/3, args_foldg/3, args_folds/3, set_env/4]).
-export([sleep_forever/0]).
-export([start_rec/1, start_rec/2]).
-export([write_pidfile/1]).
-export([chdir/0, chdir/1]).
-export([setup_logging/2]).
-export([distributed/1, distributed/2, distributed/3]).
-export([distributed_start/0, distributed_stop/0]).
-export([send_one_command/3, send_one_command/4, retry_send_one_command/4]).

-export_type([event_filter_fun/0, log_destination/0]).

%%%---------------------------------------------------------------------------
%%% types
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% log_destination() {{{

-type log_destination() ::
    stdout | stderr
  | {stdout, color|colour} | {stderr, color|colour}
  | {file, file:filename()} % what about log rotation?
  | syslog
  | {syslog, inet:hostname() | inet:ip_address()}
  | {syslog, {inet:hostname() | inet:ip_address(), inet:port_number()}}
  | lager
  | {lager, Config :: term()}
  | {gen_event, module(), Args :: term()}.

%% @type log_destination() =
%%     stdout | stderr
%%   | {stdout, color|colour} | {stderr, color|colour}
%%   | {file, file:filename()}
%%   | syslog
%%   | {syslog, inet:hostname() | inet:ip_address()}
%%   | {syslog, {inet:hostname() | inet:ip_address(), inet:port_number()}}
%%   | lager
%%   | {lager, Config :: term()}
%%   | {gen_event, module(), Args :: term()}.
%%
%% Log destination description.
%%
%% <ul>
%%   <li>`stdout' and `stderr' prints events on screen (`{StdX,colour}'
%%       additionally colours warnings and errors)</li>
%%   <li>`{file,Filename}' writes events to log file</li>
%%   <li>`syslog' sends events to local syslog daemon (facility
%%       <i>daemon</i>)</li>
%%   <li>`{syslog,Host}' and `{syslog,{Host,Port}}' send logs to remote host
%%       using syslog protocol (UDP-based) (facility <i>daemon</i>)</li>
%%   <li>`lager' starts <a href="https://github.com/basho/lager">Lager</a>
%%       application; Lager must be configured beforehand</li>
%%   <li>`{lager,Config}' starts Lager, but also configures it by calling
%%       `application:set_env(lager, handlers, Config)'</li>
%%   <li>`{gen_event,Module,Args}' adds custom module of {@link gen_event}
%%       behaviour to `error_logger' process. Remember to handle all messages
%%       specified in {@link error_logger} module documentation
%%       (here's {@link indira_log:error_logger_event(). local copy}).</li>
%% </ul>

%% }}}
%%----------------------------------------------------------
%% event_filter_fun() {{{

-type event_filter_fun() ::
  fun((indira_log:error_logger_event()) ->
        ok | {replace, indira_log:error_logger_event()} | ignore).
%% Function that filters events before entering log handler. The function can
%% decide to pass the event unchanged, to alter the event or to ignore it
%% altogether. The decision concerns just the log destination attached to this
%% filter. All the other destinations will process the original event.
%%
%% This function <i>is not</i> intended for heavy processing, like summarizing
%% events or compressing them to a single complex event. It should be as fast
%% as possible, and processing multiple events should be done by external
%% tools.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% API for running from command line (`erl -s indira')
%%%---------------------------------------------------------------------------

%% @doc Start Indira application. Function intended to be called from command
%%   line:
%% ```
%% $ erl \
%%     -indira listen '[{indira_tcp, {any,5500}}]' \
%%     -indira command '{some_module, []}' \
%%     -s indira \
%%     other args ...
%% '''

-spec start() ->
  ok | {error, term()}.

start() ->
  application:start(indira, permanent).

%%%---------------------------------------------------------------------------
%%% API for escript
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% set application configuration parameters (environment) {{{

%% @doc Set configuration option for specified application.
%%
%%   Raises an exception (`erlang:error()') on application loading error.

-spec set_option(atom(), atom(), term()) ->
  ok | no_return().

set_option(App, Option, Value) ->
  case application:load(App) of
    ok -> ok;
    {error, {already_loaded, App}} -> ok;
    {error, Reason} -> erlang:error(Reason)
  end,
  application:set_env(App, Option, Value).

%% @doc Parse command line arguments using {@link indira_opts:foldg/3}.
%%
%% @see indira_opts:foldg/3

-spec args_foldg(fun(), term(), [string()]) ->
  {ok, term()} | {error, term()}.

args_foldg(Fun, Acc, AccList) ->
  indira_opts:foldg(Fun, Acc, AccList).

%% @doc Parse command line arguments using {@link indira_opts:folds/3}.
%%
%% @see indira_opts:folds/3

-spec args_folds(fun(), term(), [string()]) ->
  {ok, term()} | {error, term()}.

args_folds(Fun, Acc, AccList) ->
  indira_opts:folds(Fun, Acc, AccList).

%% @doc Populate application environment with config loaded from file, using
%%   {@link indira_opts:set_env/4}.
%%
%% @see indira_opts:set_env/4

-spec set_env(fun(), fun(), term(), [tuple()]) ->
  ok | {error, term()}.

set_env(ConfigGet, Validate, Config, SetSpecs) ->
  indira_opts:set_env(ConfigGet, Validate, Config, SetSpecs).

%% }}}
%%----------------------------------------------------------
%% sleep forever {{{

%% @doc Sleep forever. Function intended for use in `main()' function in
%%   `escript' code.

-spec sleep_forever() ->
  no_return().

sleep_forever() ->
  % FIXME: no code release on Indira code upgrade
  receive
    % ignore all the messages (none should arrive, anyway)
    _Any -> sleep_forever()
  end.

%% }}}
%%----------------------------------------------------------
%% start application (recursively) {{{

%% @doc Start application along with all its dependencies.
%%   This function is intended for being run from `escript' code.
%%
%%   <b>NOTE</b>: Application start type is `permanent', which is a different
%%   default than {@link application:start/1} has.
%%
%% @see application:start/2

-spec start_rec(atom()) ->
  ok | {error, term()}.

start_rec(App) ->
  % defaults to the same as application:start()
  start_rec(App, permanent).

%% @doc Start application along with all its dependencies.
%%   This function is intended for being run from `escript' code.
%%
%% @see application:start/2

-spec start_rec(atom(), permanent | transient | temporary) ->
  ok | {error, term()}.

start_rec(App, StartType) ->
  case application:start(App, StartType) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok;
    {error, {not_started, AppDep}} ->
      ok = start_rec(AppDep, StartType),
      start_rec(App, StartType);
    {error, Reason} ->
      {error, Reason}
  end.

%% }}}
%%----------------------------------------------------------
%% write pidfile {{{

%% @doc Write PID file.
%%   The file will be removed on shutdown.

-spec write_pidfile(file:filename() | undefined) ->
  ok.

write_pidfile(undefined) ->
  ok;
write_pidfile(Filename) ->
  ok = set_option(indira, pidfile, Filename).

%% }}}
%%----------------------------------------------------------
%% `cd /' {{{

%% @doc Change directory to <tt>/</tt>.

-spec chdir() ->
  ok.

chdir() ->
  ok = file:set_cwd("/").

%% @doc Change directory.

-spec chdir(file:filename()) ->
  ok.

chdir(Directory) ->
  ok = file:set_cwd(Directory).

%% }}}
%%----------------------------------------------------------
%% setup logging {{{

%% @doc Setup logging.
%%   In addition to setting up logging sinks, Indira can redirect
%%   `standard_io' to {@link error_logger} (option `redirect_stdio').
%%
%%   `DaemonName' is a name of this daemon. It's used for shared logging
%%   infrastructure, like syslog.
%%
%%   Indira tries to start all the logging channels. Errors, if any, are
%%   reported at the very end by returning `{error,Errors}' tuple with list
%%   of problems. The errors will be also logged to {@link error_logger}.
%%   Logging errors are not considered critical by Indira. User can, of
%%   course, make them critical by appropriately treating `{error,_}' result.
%%
%%   <b>NOTE</b>: Indira assumes here that a single Erlang VM only hosts
%%   a single daemon, that is, there is one main function of the VM instance.
%%   There could be other functions, but they're considered auxiliary.

-spec setup_logging(atom() | string(),
                    [redirect_stdio | log_destination() |
                      {filter, event_filter_fun(), log_destination()}]) ->
  ok | {error, [Reason :: term()]}.

setup_logging(DaemonName, Options) when is_atom(DaemonName) ->
  setup_logging(atom_to_list(DaemonName), Options);

setup_logging(DaemonName, Options) ->
  % clear all the handlers (except error_logger)
  [error_logger:delete_report_handler(H) ||
    H <- gen_event:which_handlers(error_logger), H =/= error_logger],

  Results = [
    register_log_dest(DaemonName, D) || D <- Options, D =/= redirect_stdio
  ],

  % TODO: handle `redirect_stdio' option

  case [E || {error,E} <- Results] of
    [] ->
      ok;
    Errors ->
      indira_log:error(setup_logging, Errors),
      {error,Errors}
  end.

%% @doc Register an event handler in {@link error_logger}.

-spec register_log_dest(string(),
                        {filter, event_filter_fun(), log_destination()} |
                        log_destination()) ->
  ok | {error, term()}.

%% filter + log destination
register_log_dest(_DaemonName, {filter, _FilterFunc, _LogDest}) ->
  {error, {not_implemented,filter}}; % TODO

%% stdout/stderr log destination
register_log_dest(_DaemonName, stdout) ->
  Options = [{iodev, user}],
  error_logger:add_report_handler(indira_log_tty_h, Options);
register_log_dest(_DaemonName, stderr) ->
  Options = [{iodev, standard_error}],
  error_logger:add_report_handler(indira_log_tty_h, Options);
register_log_dest(_DaemonName, {stdout,colour}) ->
  Options = [{iodev, user}, {colour, true}],
  error_logger:add_report_handler(indira_log_tty_h, Options);
register_log_dest(_DaemonName, {stderr,colour}) ->
  Options = [{iodev, standard_error}, {colour, true}],
  error_logger:add_report_handler(indira_log_tty_h, Options);
register_log_dest(DaemonName, {stdout,color}) ->
  register_log_dest(DaemonName, {stdout,colour});
register_log_dest(DaemonName, {stderr,color}) ->
  register_log_dest(DaemonName, {stderr,colour});

%% file log destination
register_log_dest(DaemonName, {file, Filename}) ->
  Options = [DaemonName, Filename],
  error_logger:add_report_handler(indira_log_file_h, Options);

%% local syslog (/dev/log)
register_log_dest(DaemonName, syslog) ->
  Options = [DaemonName, daemon],
  error_logger:add_report_handler(indira_log_syslog_h, Options);
%% remote syslog (host+port)
register_log_dest(DaemonName, {syslog, {_Host, _Port} = Destination}) ->
  Options = [DaemonName, daemon, Destination],
  error_logger:add_report_handler(indira_log_syslog_h, Options);
%% remote syslog (host, default port)
register_log_dest(DaemonName, {syslog, Host}) ->
  Options = [DaemonName, daemon, {Host, default}],
  error_logger:add_report_handler(indira_log_syslog_h, Options);

%% lager
register_log_dest(_DaemonName, lager) ->
  % this is enough (lager should already be configured)
  start_rec(lager);
register_log_dest(_DaemonName, {lager, LagerHandlers}) ->
  case application:load(lager) of
    ok ->
      application:set_env(lager, handlers, LagerHandlers),
      start_rec(lager);
    {error,{already_loaded,lager}} ->
      application:set_env(lager, handlers, LagerHandlers),
      start_rec(lager);
    {error,_Reason} = Error ->
      Error
  end;

register_log_dest(_DaemonName, {gen_event, Module, Args}) ->
  error_logger:add_report_handler(Module, Args).

%% }}}
%%----------------------------------------------------------
%% distributed Erlang {{{

%% @doc Configure Erlang networking (distributed Erlang).
%%
%%   Starting the networking is delayed until {@link distributed_start/0} is
%%   called.
%%
%%   Default hostname type is `longnames'.

-spec distributed(node()) ->
  ok | {error, term()}.

distributed(Name) ->
  distributed(Name, longnames).

%% @doc Configure Erlang networking (distributed Erlang).
%%
%%   Starting the networking is delayed until {@link distributed_start/0} is
%%   called.

-spec distributed(node(), shortnames | longnames) ->
  ok | {error, term()}.

distributed(Name, NameType) ->
  distributed(Name, NameType, none).

%% @doc Configure Erlang networking (distributed Erlang).
%%
%%   Starting the networking is delayed until {@link distributed_start/0} is
%%   called.
%%
%%   When providing cookie in the form of `{file,Path}', only the first line
%%   (with `"\n"' stripped) will be used as cookie.

-spec distributed(node(), shortnames | longnames, Cookie) ->
  ok | {error, term()}
  when Cookie :: atom() | string() | binary() | {file, file:filename()}.

distributed(Name, NameType, {file, CookieFile} = _Cookie) ->
  case file:read_file(CookieFile) of
    {ok, <<>>} ->
      {error, no_cookie};
    {ok, <<"\n", _/binary>>} ->
      {error, no_cookie};
    {ok, <<"\r\n", _/binary>>} ->
      {error, no_cookie};
    {ok, CookieFileContent} ->
      [CookieBin | _] = binary:split(CookieFileContent, <<"\n">>),
      distributed(Name, NameType, CookieBin);
    {error, Reason} ->
      {error, Reason}
  end;

distributed(Name, NameType, Cookie) when is_list(Cookie) ->
  distributed(Name, NameType, list_to_atom(Cookie));

distributed(Name, NameType, Cookie) when is_binary(Cookie) ->
  distributed(Name, NameType, binary_to_atom(Cookie, utf8));

distributed(Name, NameType, Cookie) when is_atom(Cookie) ->
  case NameType of
    shortnames -> ok = set_option(indira, net, {Name, NameType, Cookie});
    longnames  -> ok = set_option(indira, net, {Name, NameType, Cookie});
    _ -> {error, badarg}
  end.

%% @doc Start Erlang networking, as configured through {@link distributed/3}.

-spec distributed_start() ->
  ok | {error, term()}.

distributed_start() ->
  indira_dist_erl:bring_up().

%% @doc Stop Erlang networking.

-spec distributed_stop() ->
  ok | {error, term()}.

distributed_stop() ->
  indira_dist_erl:tear_down().

%% }}}
%%----------------------------------------------------------
%% send an administrative command to a daemon instance {{{

%% @doc Open a connection to daemon's Indira, send a command, receive a reply.
%%
%%   `Module' implements {@link gen_indira_listener} behaviour, and `Address'
%%   is (typically) the same tuple as one would specify for listening with
%%   that module.

-spec send_one_command(module(), term(), indira_json:struct()) ->
  {ok, indira_json:struct()} | {error, badarg | bad_reply | term()}.

send_one_command(Module, Address, Command) ->
  send_one_command(Module, Address, Command, infinity).

%% @doc Open a connection to daemon's Indira, send a command, receive a reply.
%%
%%   `Module' implements {@link gen_indira_listener} behaviour, and `Address'
%%   is (typically) the same tuple as one would specify for listening with
%%   that module.

-spec send_one_command(module(), term(), indira_json:struct(), timeout()) ->
  {ok, indira_json:struct()} | {error, badarg | bad_reply | term()}.

send_one_command(Module, Address, Command, Timeout) ->
  case indira_json:encode(Command) of
    {ok, Line} ->
      case Module:send_one_line(Address, Line, Timeout) of
        {ok, ReplyLine} ->
          case indira_json:decode(ReplyLine) of
            {ok, Reply} ->
              {ok, Reply};
            {error, badarg} ->
              {error, bad_reply}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, badarg} ->
      {error, badarg}
  end.

%% @doc Open a connection to daemon's Indira, send a command, receive a reply;
%%   retry on refused connection.
%%
%%   `Module' implements {@link gen_indira_listener} behaviour, and `Address'
%%   is (typically) the same tuple as one would specify for listening with
%%   that module.

-spec retry_send_one_command(module(), term(), indira_json:struct(),
                             timeout()) ->
  {ok, indira_json:struct()} | {error, badarg | bad_reply | term()}.

retry_send_one_command(Module, Address, Command, Timeout) ->
  case indira_json:encode(Command) of
    {ok, Line} ->
      case Module:retry_send_one_line(Address, Line, Timeout) of
        {ok, ReplyLine} ->
          case indira_json:decode(ReplyLine) of
            {ok, Reply} ->
              {ok, Reply};
            {error, badarg} ->
              {error, bad_reply}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, badarg} ->
      {error, badarg}
  end.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
