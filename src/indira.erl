%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira main API.
%%%
%%%   This module contains functions to be called from `escript' script and
%%%   utility functions for {@link gen_indira_listener. listeners}. It also
%%%   documents how to configure Indira ({@section Indira configuration}) and
%%%   the protocol to communicate with {@section Command executor}.
%%%
%%%   == Indira configuration ==
%%%
%%%   Indira uses two configuration keys ({@link application:get_env/2}):
%%%   <i>listen</i> and <i>commander</i>.
%%%
%%%   The value of <i>commander</i> is a name (atom) under which your
%%%   {@section Command executor} will be registered. If you configure Indira
%%%   with {@link application:set_env/3}, you can provide raw PID, but it's
%%%   generally not a good idea (think of dying processes).
%%%
%%%   The value of <i>listen</i> is a list of pairs `{Mod,Args}'. `Mod' is
%%%   a name (atom) of module which is considered to be listener's entry
%%%   point. `Args' is an argument passed to `Mod:supervision_child_spec/2'.
%%%   For more details see {@link gen_indira_listener}.
%%%
%%%   A file passed to <i>-config</i> VM option could look like this:
%%%   ```
%%%   [
%%%     {indira, [
%%%       {listen, [
%%%         {indira_tcp, {"localhost", 16667}},
%%%         {indira_unix, "/var/run/my_app.sock"}
%%%       ]},
%%%       {commander, my_app_command}
%%%     ]},
%%%     % ...
%%%   ].
%%%   '''
%%%
%%%   == Command executor ==
%%%
%%%   Command executor is a process that receives messages of following
%%%   structure: `{command,ReplyTo,ChannelID,Command}'. It should reply with
%%%   tuple `{result,ChannelID,Reply}' to `ReplyTo' process.
%%%
%%%   Data in `Reply' should be jsx-like structured. Short reference is here:
%%%   {@section Data structure (jsx)}.
%%%
%%%   Data in `Command' has either jsx-like structure if the command was
%%%   encoded as JSON ({@section JSON command}) or is a tuple if the command
%%%   was a {@section simple command}.
%%%
%%%   Command executor is responsible for logging commands it has received.
%%%   It's highly recommended to log the commands being executed in the
%%%   application.
%%%
%%%   Some proposals for commands:
%%%   <ul>
%%%     <li><i>stop</i>, shutdown the daemon (initscript)</li>
%%%     <li><i>is daemon running?</i>, wait for daemon to start
%%%         (initscript)</li>
%%%     <li><i>reload</i>, reload configuration stored in a file</li>
%%%     <li><i>netconfig</i>, setup distributed Erlang (epmd, cookie and
%%%         <i>-(s)name</i></li>)
%%%   </ul>
%%%
%%%   === Example executor ===
%%%
%%%   This is the simplest (and the dumbest) command executor:
%%%   ```
%%%   start_command_executor() ->
%%%     spawn(fun() ->
%%%       register(my_app_command, self()),
%%%       command_executor()
%%%     end).
%%%
%%%   command_executor() ->
%%%     receive
%%%       {command, ReplyTo, ChannelID, Command} ->
%%%         error_logger:info_report(my_app_command, [
%%%           {message, "got a command"},
%%%           {command, Command}, {channel, ChannelID}
%%%         ]),
%%%         ReplyTo ! {result, ChannelID, ok},
%%%         command_executor();
%%%       _Any ->
%%%         % ignore
%%%         command_executor()
%%%     end.
%%%   '''
%%%
%%%   For real world applications, the executor will probably implement
%%%   {@link gen_server} behaviour and be started as a part of application's
%%%   supervision tree.
%%%
%%%   Note: when handling commands, remember to add support for unrecognized
%%%   commands. You don't want your command executor crashed when talking to
%%%   buggy client.
%%%
%%%   == Communication protocol ==
%%%
%%%   The protocol to communicate with the world outside of Erlang VM is
%%%   line-based. There can be various ways of delivering the line (e.g. HTTP
%%%   POST); they won't be covered here. Instead, this section focuses on the
%%%   format of the line itself.
%%%
%%%   Request line is either a simple command or JSON document.
%%%
%%%   When command executor sent something that couldn't be serialized, the
%%%   response line will be "`bad result'". Otherwise, the response will
%%%   always be single-line JSON document.
%%%
%%%   === simple command ===
%%%
%%%   A simple command line is composed of a word denoting the command and
%%%   list of options (possibly empty) separated with spaces. Option is a word
%%%   or assignment. Assignment is composed of word, "`='" sign and a word,
%%%   JSON string or JSON number. Word is a sequence of letters, digits,
%%%   underscore, hyphen and period, beginning with a letter or underscore.
%%%
%%%   Some examples:
%%%   ```
%%%   command_word
%%%   also.a.word this_is_an_option
%%%   some_cmd option=value option2="spaces and\nnewlines allowed in strings"
%%%   '''
%%%
%%%   Simple command is sent to command executor as a tuple of
%%%   `{<<"command">>, Options}', where `Options' is a list (possibly empty)
%%%   of binaries (word option) or pairs `{Name,Value}' (assignment; `Name'
%%%   will be a binary, `Value' will be a binary, integer or float, depending
%%%   on the assigned value).
%%%
%%%   Commands from example above will be passed to command executor as:
%%%   ```
%%%   {<<"command_word">>, []}.
%%%   {<<"also.a.word">>, [<<"this_is_an_option">>]}.
%%%   {<<"some_cmd">>, [
%%%     {<<"option">>, <<"value">>},
%%%     {<<"option2">>, <<"spaces and\nnewlines allowed in strings">>}
%%%   ]}.
%%%   '''
%%%
%%%   === JSON command ===
%%%
%%%   JSON command consists of a valid single-line JSON document.
%%%
%%%   Erlang representation is based on jsx.
%%%   <ul>
%%%     <li><i>object</i> is encoded as `[{}]' (empty object) or a list of
%%%         `{Key,Value}' pairs, where `Key' is always a binary and `Value' is
%%%         JSON value</li>
%%%     <li><i>array</i> is encoded as a list</li>
%%%     <li><i>string</i> is encoded as a binary (UTF-8 encoding)</li>
%%%     <li><i>number</i> is encoded as integer (if possible) or float</li>
%%%     <li>`true', `false' and `null' are encoded as corresponding atoms</li>
%%%   </ul>
%%%
%%%   === Data structure (jsx) ===
%%%
%%%   Serializable data is:
%%%   <ul>
%%%     <li>`[{}]', encoded as empty JSON object</li>
%%%     <li>`[{K,V}, ...]', encoded as JSON object; `V' can be any
%%%         serializable data, `K' is an atom, string (list) or a binary</li>
%%%     <li>`[]', `[V,...]', encoded as a JSON array; `V' can be any
%%%         serializable data</li>
%%%     <li>integer is encoded as a number (no periods or "e" letters)</li>
%%%     <li>float is encoded as a number (containing period, "e", or
%%%         both)</li>
%%%     <li>atoms `null', `true' and `false' are encoded as corresponding JSON
%%%         values</li>
%%%     <li>atom other than above-mentioned three is encoded as a string</li>
%%%     <li>binary is encoded as a string</li>
%%%   </ul>
%%%   Note that Erlang strings are generally recognized as arrays of integers,
%%%   except for being a key in proplist.
%%%
%%%   Note also that tuples alone are not allowed. They can only compose
%%%   proplists that will be serialized to JSON object.
%%%
%%% @TODO API for client command line utilities.
%%% @TODO Notice if no listeners defined.
%%% @TODO Channel types defined by operator (to differentiate between unix
%%%   socket, SSL and TCP).
%%% @TODO Pre-format standard OTP logs.
%%% @TODO Add some channel-defined context to commands (e.g. for operation
%%%   logging).
%%%
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
-export([set_environment/1, set_option/3, load_app_config/1]).
-export([sleep_forever/0]).
-export([start_rec/1, start_rec/2]).
-export([write_pidfile/1]).
-export([chdir/0, chdir/1]).
-export([setup_logging/2]).
-export([distributed/1]).
-export([distributed/2]).
-export([distributed/3]).

%% API for listeners
-export([command/2, command/3]).
-export([log_info/2, log_error/2, log_error/3,
         log_critical/2, log_critical/3]).

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
%%% API for running from command line (`erl -s indira')
%%%---------------------------------------------------------------------------

%% @doc Start Indira application. Function intended to be called from command
%%   line:
%% ```
%% $ erl \
%%     -indira listen '[{indira_tcp, {any,5500}}]' \
%%     -indira commander some_module \
%%     -s indira \
%%     other args ...
%% '''
start() ->
  application:start(indira, permanent).

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

%% @doc Load application configuration file (suitable for <i>-config</i> VM
%%   option).
%%
%%   Note that <i>unloading</i> an application mentioned in the file loaded by
%%   this function may make the configuration to be lost. Please don't unload
%%   applications.

-spec load_app_config(string()) ->
  ok | {error, term()}.

load_app_config(File) ->
  case file:consult(File) of
    % AppConfigList :: [ {AppName :: atom(), [ {K :: atom(), V :: term()} ]} ]
    {ok, [AppConfigList]} ->
      load_app_config_list(AppConfigList);
    {ok, _} ->
      {error, badformat};
    {error, _Reason} = Error ->
      Error
  end.

%% @doc Load applications and set their config variables according to the
%%   <i>-config</i> list.

-spec load_app_config_list([ {atom(), [{atom(), term()}]} ]) ->
  ok | {error, term()}.

load_app_config_list([] = _AppConfigList) ->
  ok;
load_app_config_list([{AppName, Config} | Rest]) ->
  case application:load(AppName) of
    ok ->
      [application:set_env(AppName, K, V) || {K,V} <- Config],
      load_app_config_list(Rest);
    {error, {already_loaded, AppName}} ->
      [application:set_env(AppName, K, V) || {K,V} <- Config],
      load_app_config_list(Rest);
    {error, _Reason} = Error ->
      % stop here
      Error
  end.

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
    {error, {not_started, AppDep}} ->
      ok = start_rec(AppDep, StartType),
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
%% write pidfile {{{

%% @doc Write PID file.
%% @TODO Remove PID file when shutting down.
write_pidfile(undefined) ->
  ok;
write_pidfile(Filename) ->
  Pid = iolist_to_binary([os:getpid(), "\n"]),
  ok = file:write_file(Filename, Pid).

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
%%   Indira tries to start all the logging channels. Errors, if any, are
%%   reported at the very end by returning `{error,Errors}' tuple with list
%%   of problems. The errors will be also logged to {@link error_logger}.
%%   Logging errors are not considered critical by Indira. User can, of
%%   course, make them critical by appropriately treating `{error,_}' result.
%%
%%   <b>NOTE</b>: Indira assumes here that a single Erlang VM only hosts
%%   a single daemon, that is, there is one main function of the VM instance.
%%   There could be other functions, but they're considered auxiliary.
%%
%% @spec setup_logging(atom() | string(),
%%                     [redirect_stdio | log_destination()
%%                       | {filter, event_filter_fun(), log_destination()}]) ->
%%   ok | {error, [Reasons]}

-spec setup_logging(atom() | string(),
                    [redirect_stdio | log_destination()
                      | {filter, event_filter_fun(), log_destination()}]) ->
  ok | {error, [term()]}.

setup_logging(DaemonName, Options) when is_atom(DaemonName) ->
  setup_logging(atom_to_list(DaemonName), Options);

setup_logging(DaemonName, Options) ->
  % clear all the handlers (except error_logger)
  [error_logger:delete_report_handler(H) ||
    H <- gen_event:which_handlers(error_logger), H =/= error_logger],

  Results =
    [register_log_dest(DaemonName, D) || D <- Options, D =/= redirect_stdio],

  % TODO: handle `redirect_stdio' option

  case [E || {error,E} <- Results] of
    [] ->
      ok;
    Errors ->
      log_error(setup_logging, Errors),
      {error,Errors}
  end.

%% @doc Register an event handler in {@link error_logger}.

-spec register_log_dest(string(),
  {filter, event_filter_fun(), log_destination()} | log_destination()) ->
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
register_log_dest(_DaemonName, {file, _Filename}) ->
  {error, {not_implemented,file}}; % TODO

%% syslog log destination
register_log_dest(DaemonName, syslog) ->
  Options = [{facility, daemon}, {ident, DaemonName}],
  error_logger:add_report_handler(indira_log_syslog_h, Options);
register_log_dest(DaemonName, {syslog, Destination}) ->
  Options = [{remote, Destination}, {facility, daemon}, {ident, DaemonName}],
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
distributed(Name) ->
  distributed(Name, longnames).

%% @doc Configure Erlang networking (distributed Erlang).
distributed(Name, NameType) ->
  % NameType :: shortnames | longnames
  net_kernel:start([Name, NameType]).

%% @doc Configure Erlang networking (distributed Erlang).
%% @TODO Support for `{file, CookieFile}'
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
%%   According to client protocol, response to the command from executor will
%%   be returned as a message to the process that called this function.
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
%%   This function is intended for use with single `{error,Reason}' tuple.
%%
%%   Such error is something that generally shouldn't happen, but if it does,
%%   it has limited scope (e.g. command line parse error for a single client).
log_error(ErrorType, ErrorReason, Context) ->
  error_logger:warning_report(
    [{indira_error, ErrorType}, {error, ErrorReason} | Context]
  ).

%% @doc Send error report about an error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use when no single `{error,Reason}' tuple
%%   is present (e.g. when multiple errors occurred).
%%
%%   Such error is something that generally shouldn't happen, but if it does,
%%   it has limited scope (e.g. command line parse error for a single client).
log_error(ErrorType, Context) ->
  error_logger:warning_report([{indira_error, ErrorType} | Context]).

%% @doc Send log report about severe error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use with single `{error,Reason}' tuple.
%%
%%   For severe errors, like inability to listen on a specified port.
log_critical(ErrorType, ErrorReason, Context) ->
  error_logger:error_report(
    [{indira_error, ErrorType}, {error, ErrorReason} | Context]
  ).

%% @doc Send log report about severe error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use when no single `{error,Reason}' tuple
%%   is present (e.g. when multiple errors occurred).
%%
%%   For severe errors, like inability to listen on a specified port.
log_critical(ErrorType, Context) ->
  error_logger:error_report([{indira_error, ErrorType} | Context]).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
