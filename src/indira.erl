%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira main API.
%%%
%%% @TODO Describe how does environment config ({@link application:get_env/2})
%%%   look like (`application:get_env(indira, listen)' is a list of tuples
%%%   `{Module, ListenerArg}').
%%% @TODO Describe communication protocol between Indira and command executor.
%%%   Remember that command executor should log commands, as Indira doesn't do
%%%   that.
%%%
%%% @see indira_tcp
%%% @see indira_udp
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(indira).

%% API for escript
-export([set_environment/1, set_option/3, load_app_config/1]).
-export([sleep_forever/0]).
-export([start_rec/1, start_rec/2]).
-export([load_plugins_dir/1]).
-export([write_pidfile/1]).
-export([chdir/0, chdir/1]).
-export([setup_logging/2]).
-export([distributed/1]).
-export([distributed/2]).
-export([distributed/3]).

%% API for listeners
-export([command/2, command/3]).
-export([log_info/2, log_error/3, log_critical/3]).

%%%---------------------------------------------------------------------------
%%% types
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% log_destination() {{{

%% @type log_destination() =
%%     stdout | stderr
%%   | {stdout, color|colour} | {stderr, color|colour}
%%   | {file, Filename :: string()}
%%   | syslog
%%   | {syslog, DestHost :: inet:hostname() | inet:ip_address()}
%%   | {syslog, {Host :: inet:hostname() | inet:ip_address(),
%%                Port :: integer()}}
%%   | lager
%%   | {lager, Config :: term()}
%%   | {gen_event, Module :: term(), Args :: term()}.
%%
%% Log destination description.
%% <ul>
%%   <li>`stdout' and `stderr' prints events on screen (`{StdX,colour}'
%%       additionally colours warnings and errors)</li>
%%   <li>`{file,Filename}' writes events to log file</li>
%%   <li>`syslog' sends events to local syslog daemon</li>
%%   <li>`{syslog,Host}' and `{syslog,{Host,Port}}' send logs to remote host
%%       using syslog protocol (UDP-based)</li>
%%   <li>`lager' starts <a href="https://github.com/basho/lager">Lager</a>
%%       application; Lager must be configured beforehand</li>
%%   <li>`{lager,Config}' starts Lager, but also configures it by calling
%%       `application:set_env(lager, handlers, Config)'</li>
%%   <li>`{gen_event,Module,Args}' adds custom module of {@link gen_event}
%%       behaviour to `error_logger' process. Remember to handle all messages
%%       specified in {@link error_logger} module documentation
%%       (here's {@link error_logger_event(). local copy}).</li>
%% </ul>

-type log_destination() ::
    stdout | stderr
  | {stdout, color|colour} | {stderr, color|colour}
  | {file, Filename :: string()} % what about log rotation?
  | syslog
  | {syslog, DestHost :: atom() | string() | inet:ip_address()}
  | {syslog, {Host :: atom() | string() | inet:ip_address(),
               Port :: integer()}}
  | lager
  | {lager, Config :: term()}
  | {gen_event, Module :: term(), Args :: term()}.

%% }}}
%%----------------------------------------------------------
%% error_logger_event() {{{

%% @type error_logger_event() =
%%     {info_msg | warning_msg | error,
%%       GroupLeader :: pid(),
%%       {EventOrigin :: pid(), Format :: string() | atom(), Data :: list()}}
%%   | {info_report | warning_report | error_report,
%%       {EventOrigin :: pid(), Type :: atom(), Report :: term()}}.
%%
%% Event structure extracted from {@link error_logger} documentation.
%%
%% Note that while {@link error_logger:info_msg/1} and
%% {@link error_logger:warning_msg/1} produce `*_msg' events, 
%% {@link error_logger:error_msg/1} produces just `error'.
%% `error_report' keeps the convention, however.
%%
%% `EventOrigin' is the process that generated event (that is, called
%% {@link error_logger:info_msg/1} or its companion).
%%
%% `Type' is a type of report. Some typical values include:
%% <ul>
%%   <li>`std_info', `std_warning', `std_error' for
%%       {@link error_logger:info_report/1},
%%       {@link error_logger:warning_report/1},
%%       {@link error_logger:error_report/1}</li>
%%   <li>`progress' for application start report</li>
%%   <li>`supervisor_report' for errors from supervisor</li>
%% </ul>
%% `Type' in error report can be specified by calling
%% {@link error_logger:error_report/2} (and similar thing for info and
%% warning).

-type error_logger_event() ::
    {info_msg | warning_msg | error,
      GroupLeader :: pid(),
      {EventOrigin :: pid(), Format :: string() | atom(), Data :: list()}}
  | {info_report | warning_report | error_report,
      {EventOrigin :: pid(), Type :: atom(), Report :: term()}}.

%% }}}
%%----------------------------------------------------------
%% event_filter_fun() {{{

%% @type event_filter_fun() =
%%   fun(
%%     (error_logger_event()) ->
%%       ok | {replace, error_logger_event()} | ignore
%%   ).
%%
%% Function that filters events before entering log handler. The function can
%% decide to pass the event unchanged, to alter the event or to ignore it
%% altogether. The decision concerns just the log destination attached to this
%% filter. All the other destinations will process the original event.
%%
%% This function <i>is not</i> intended for heavy processing, like summarizing
%% events or compressing them to a single complex event. It should be as fast
%% as possible, and processing multiple events should be done by external
%% tools.

-type event_filter_fun() ::
  fun((error_logger_event()) -> ok | {replace, error_logger_event()} | ignore).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% API for escript
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% set application configuration parameters (environment) {{{

%% @doc Set list of configuration options for appropriate applications.
set_environment([]) ->
  ok;
set_environment([{App, Opt, Value} | Options]) ->
  set_option(App, Opt, Value),
  set_environment(Options).

%% @doc Set configuration option for specified application.
set_option(App, Option, Value) ->
  case application:load(App) of
    ok -> ok;
    {error, {already_loaded, App}} -> ok;
    {error, Reason} -> erlang:error(Reason)
  end,
  application:set_env(App, Option, Value).

%% @doc Load application configuration file (suitable for `-config' VM
%%   option).
load_app_config(_File) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% sleep forever {{{

%% @doc Sleep forever. Function intended for use in `main()' function in
%%   `escript' code.
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
%%   <b>NOTE</b>: Application start type is `permanent'.
%% @see application:start/2
start_rec(App) ->
  % defaults to the same as application:start()
  start_rec(App, permanent).

%% @doc Start application along with all its dependencies.
%%   This function is intended for being run from `escript' code.
%% @see application:start/2
start_rec(App, StartType) ->
  case application:start(App, StartType) of
    {error, {not_started, App1}} ->
      ok = start_rec(App1, StartType),
      start_rec(App, StartType);
    {error, {already_started, App}} ->
      ok;
    {error, _Any} = Error ->
      Error;
    ok ->
      ok
  end.

%% }}}
%%----------------------------------------------------------
%% load plugins (BEAM files) from directory {{{

%% @TODO Compile + load function.
%% @TODO Load plugins from multiple directories.
%% @TODO Reload plugins from multiple directories.
%%   Hint:
%%   ```
%%   [T || {_,F} = T <- code:all_loaded(), is_list(F), lists:prefix(P,F)]
%%   '''
%%   {@link code:purge/1}, {@link code:soft_purge/1}
%% @TODO Problem with reload: somebody wants to run something between purge
%%   and load (race condition).

%% @doc Load <tt>*.beam</tt> files from specified directory.
load_plugins_dir(Directory) ->
  Files = filelib:fold_files(
    Directory, "\\.beam$", false,
    fun(F, Acc) ->
      AF = filename:absname(F),
      CF = filename:rootname(AF),
      {module, Mod} = code:load_abs(CF),
      [{Mod, AF} | Acc]
    end, []
  ),
  Files.

%% }}}
%%----------------------------------------------------------
%% write pidfile {{{

%% @doc Write PID file.
write_pidfile(undefined) ->
  ok;
write_pidfile(Filename) ->
  Pid = iolist_to_binary([os:getpid(), "\n"]),
  file:write_file(Filename, Pid).

%% }}}
%%----------------------------------------------------------
%% `cd /' {{{

%% @doc Change directory to <tt>/</tt>.
chdir() ->
  ok = file:set_cwd("/").

%% @doc Change directory.
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
%%   <b>NOTE</b>: Indira assumes here that a single Erlang VM only hosts
%%   a single daemon, that is, there is one main function of the VM instance.
%%   There could be other functions, but they're considered auxiliary.
%%
%% @spec setup_logging(atom() | string(),
%%                     [redirect_stdio | log_destination()
%%                       | {filter, event_filter_fun(), log_destination()}]) ->
%%   any()

-spec setup_logging(atom() | string(),
                    [redirect_stdio | log_destination()
                      | {filter, event_filter_fun(), log_destination()}]) ->
  any().

setup_logging(_DaemonName, _Options) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% distributed Erlang {{{

%% @doc Configure Erlang networking (distributed Erlang).
distributed(Name) ->
  distributed(Name, longnames).

%% @doc Configure Erlang networking (distributed Erlang).
distributed(Name, NameType) ->
  % NameType :: shortnames | longnames
  net_kernel:start([Name, NameType]).

%% @doc Configure Erlang networking (distributed Erlang).
distributed(Name, NameType, Cookie) ->
  net_kernel:start([Name, NameType]),
  CookieAtom = case Cookie of
    _ when is_atom(Cookie)   -> Cookie;
    _ when is_list(Cookie)   -> list_to_atom(Cookie);
    _ when is_binary(Cookie) -> binary_to_atom(Cookie, utf8)
  end,
  erlang:set_cookie(node(), CookieAtom).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% API for listeners
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% send command line to router {{{

%% @doc Send command to Indira router.
%%   The process calling this function will get the response as a message.
%%
%% @see indira_router:command/2
command(Indira, Line) ->
  indira_router:command(Indira, Line).

%% @doc Send command to Indira router.
%%   The process calling this function will get the response as a message.
%%
%%   `RoutingKey' is an additional information to tell apart between multiple
%%   clients and will be included in command reply message.
%%
%%   This call form is only needed when a single process handles multiple
%%   clients.
%%
%% @see indira_router:command/3
command(Indira, RoutingKey, Line) ->
  indira_router:command(Indira, RoutingKey, Line).

%% }}}
%%----------------------------------------------------------
%% logging (unified) {{{

%% @doc Send info report about an event to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
log_info(InfoType, Context) ->
  error_logger:info_report([{indira_info, InfoType} | Context]).

%% @doc Send error report about an error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   Such error is something that generally shouldn't happen, but if it does,
%%   it has limited scope (e.g. command line parse error for a single client).
log_error(ErrorType, ErrorReason, Context) ->
  error_logger:warning_report(
    [{indira_error, ErrorType}, {error, ErrorReason} | Context]
  ).

%% @doc Send log report about severe error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   For severe errors, like inability to listen on a specified port.
log_critical(ErrorType, ErrorReason, Context) ->
  error_logger:error_report(
    [{indira_error, ErrorType}, {error, ErrorReason} | Context]
  ).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
