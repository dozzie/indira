%%%---------------------------------------------------------------------------
%%% @doc
%%%   Functions for working with applications and their configuration.
%%%
%%% @todo Notice if no listeners defined (see {@link
%%%   indira_socket_sup:init/1}).
%%% @todo Don't crash BEAM when Indira failed to start. Rather, provide means
%%%   to detect it in startup script.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_app).

%% setting options
-export([set_env/2, default_env/1, indira_setup/1]).
-export([reload/0, format_stacktrace/1]).
%% starting applications
-export([start_rec/1, start_rec/2, daemonize/2]).
-export([wait_for_start/1, is_started/1]).
-export([sleep_forever/0]).
%% starting/stopping distributed Erlang
-export([distributed_start/0, distributed_stop/0, distributed_reconfigure/1]).

-export_type([daemon_option/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

-define(RELOAD_MUTEX_NAME, '$indira_reload').

-type daemon_option() ::
    {listen, [{module(), gen_indira_socket:listen_address()}]}
  | {command, {module(), term()}}
  | {reload, {Mod :: module(), Fun :: atom(), Args :: [term()]}}
  | {pidfile, file:filename() | undefined}
  | {node_name, node() | undefined}
  | {name_type, shortnames | longnames | undefined}
  | {cookie, none | undefined | atom() | {file, file:filename()}}
  | {net_start, boolean() | undefined}
  | {start_before, AppName :: atom() | undefined}
  | {start_after, AppName :: atom() | undefined}.
%% Options that correspond to Indira's environment. `node_name', `name_type',
%% and `cookie' compose <i>indira/net</i> parameter. See {@link configuration}
%% for details. `{start_*, AppName}' allow to start additional applications
%% (e.g. SASL) before or after Indira and add up when used multiple times.
%%
%% Setting an option to `undefined' has the same result as omitting it (other
%% instances in the option list may still set the option, though).

%%% }}}
%%%---------------------------------------------------------------------------
%%% starting/stopping distributed Erlang
%%%---------------------------------------------------------------------------

%% @doc Start Erlang networking, as configured through <i>indira/net</i>
%%   environment parameter.
%%
%%   This function <em>does not</em> start `epmd' port mapper daemon, which is
%%   needed for distributed Erlang to work. `epmd' should be started
%%   separately.

-spec distributed_start() ->
  ok | {error, term()}.

distributed_start() ->
  indira_dist_erl:bring_up().

%% @doc Stop Erlang networking.

-spec distributed_stop() ->
  ok | {error, term()}.

distributed_stop() ->
  indira_dist_erl:tear_down().

%% @doc Reconfigure Erlang networking.
%%
%%   If network is up, it's restarted. If it is down, it's left down.
%%
%%   Only network-related options are considered. All the others are ignored.
%%
%%   <i>NOTE</i>: You can't deconfigure Erlang networking this way.
%%
%% @see indira_setup/1

-spec distributed_reconfigure([daemon_option()]) ->
  ok | {error, invalid_net_config | term()}.

distributed_reconfigure(Options) ->
  case set_indira_options([net, net_start], Options) of
    ok -> indira_dist_erl:reconfigure();
    {error, Reason} -> {error, Reason}
  end.

%%%---------------------------------------------------------------------------
%%% setting options
%%%---------------------------------------------------------------------------

%% @doc Synchronize application's environment to the specified values.

-spec set_env(atom(), Environment :: [Param]) ->
  ok | {error, Reason}
  when Param :: {Name :: atom(), Value :: term()},
       Reason :: bad_name | bad_app | file:posix().

set_env(App, Environment) ->
  case default_env(App) of
    {ok, DefaultEnv} ->
      application:load(App),
      % environment is a proplist: the earlier keys overwrite the later ones
      EnvDict = lists:foldr(
        fun({Name, Value}, Acc) -> dict:store(Name, Value, Acc) end,
        dict:new(),
        Environment ++ DefaultEnv
      ),
      lists:foreach(
        fun(Name) -> application:unset_env(App, Name) end,
        [Name ||
          {Name, _Value} <- application:get_all_env(App),
          not dict:is_key(Name, EnvDict)]
      ),
      dict:fold(
        fun(Name, Value, _Acc) -> application:set_env(App, Name, Value) end,
        ignore, EnvDict
      ),
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Load application's default parameters.

-spec default_env(atom()) ->
  {ok, Environment :: [Param]} | {error, Reason}
  when Param :: {Name :: atom(), Value :: term()},
       Reason :: bad_name | bad_app | file:posix().

default_env(App) when is_atom(App) ->
  case code:lib_dir(App, ebin) of
    {error, bad_name} ->
      {error, bad_name};
    Ebin ->
      case file:consult(filename:join(Ebin, atom_to_list(App) ++ ".app")) of
        {ok, [{application, App, Spec}]} ->
          {ok, proplists:get_value(env, Spec, [])};
        {ok, _} ->
          % the file contents are of unexpected format
          {error, bad_app};
        {error, Reason} when is_tuple(Reason) ->
          % *.app file parse error
          {error, bad_app};
        {error, Reason} ->
          % most probably read error (eperm or similar)
          {error, Reason}
      end
  end.

%% @doc Set Indira application's environment parameters.
%%
%% NOTE: Setting an option to `undefined' has the same result as omitting it
%% altogether.

-spec indira_setup([daemon_option()]) ->
  ok | {error, Reason}
  when Reason :: invalid_listen_spec
               | invalid_command_handler
               | invalid_reload_function
               | invalid_pidfile
               | invalid_net_config
               | invalid_net_start.

indira_setup(Options) ->
  % TODO: handle error
  case application:load(indira) of
    ok -> ok;
    {error, {already_loaded, indira}} -> ok
  end,
  set_indira_options([listen, command, reload, pidfile, net, net_start, apps],
                     Options).

%%----------------------------------------------------------
%% validate and set Indira options {{{

%% @doc Workhorse for {@link indira_setup/1}.

-spec set_indira_options([atom()], [daemon_option()]) ->
  ok | {error, Reason}
  when Reason :: invalid_listen_spec
               | invalid_command_handler
               | invalid_reload_function
               | invalid_pidfile
               | invalid_net_config
               | invalid_net_start.

set_indira_options([listen | Rest] = _Aspects, Options) ->
  ListenSpecs = proplists:get_value(listen, Options, []),
  case check_listen_specs(ListenSpecs) of
    ok ->
      application:set_env(indira, listen, ListenSpecs),
      set_indira_options(Rest, Options);
    {error, Reason} ->
      {error, Reason}
  end;
set_indira_options([command | Rest] = _Aspects, Options) ->
  case proplists:get_value(command, Options) of
    {Mod, _Args} = CommandHandler when is_atom(Mod) ->
      application:set_env(indira, command, CommandHandler),
      set_indira_options(Rest, Options);
    CommandHandler when is_function(CommandHandler, 1) ->
      application:set_env(indira, command, CommandHandler),
      set_indira_options(Rest, Options);
    {Fun, _Args} = CommandHandler when is_function(Fun, 2) ->
      application:set_env(indira, command, CommandHandler),
      set_indira_options(Rest, Options);
    undefined ->
      set_indira_options(Rest, Options);
    _ ->
      {error, invalid_command_handler}
  end;
set_indira_options([reload | Rest] = _Aspects, Options) ->
  case proplists:get_value(reload, Options) of
    {Mod, Fun, Args} = MFA when is_atom(Mod), is_atom(Fun), is_list(Args) ->
      application:set_env(indira, reload_function, MFA),
      set_indira_options(Rest, Options);
    undefined ->
      set_indira_options(Rest, Options);
    _ ->
      {error, invalid_reload_function}
  end;
set_indira_options([pidfile | Rest] = _Aspects, Options) ->
  case proplists:get_value(pidfile, Options) of
    PidFile when is_list(PidFile) orelse is_binary(PidFile) ->
      application:set_env(indira, pidfile, PidFile),
      set_indira_options(Rest, Options);
    undefined ->
      set_indira_options(Rest, Options);
    _ ->
      {error, invalid_pidfile}
  end;
set_indira_options([net | Rest] = _Aspects, Options) ->
  NodeName = proplists:get_value(node_name, Options),
  NameType = proplists:get_value(name_type, Options),
  Cookie = case proplists:get_value(cookie, Options) of
    undefined -> none;
    C -> C
  end,
  case check_net_config(NodeName, NameType, Cookie) of
    ok ->
      application:set_env(indira, net, {NodeName, NameType, Cookie}),
      set_indira_options(Rest, Options);
    skip ->
      set_indira_options(Rest, Options);
    {error, Reason} ->
      {error, Reason}
  end;
set_indira_options([net_start | Rest] = _Aspects, Options) ->
  case proplists:get_value(net_start, Options) of
    NetStart when is_boolean(NetStart) ->
      application:set_env(indira, net_start, NetStart),
      set_indira_options(Rest, Options);
    undefined ->
      set_indira_options(Rest, Options);
    _ ->
      {error, invalid_net_start}
  end;
set_indira_options([apps | Rest] = _Aspects, Options) ->
  StartBefore = [
    A || A <- proplists:get_all_values(start_before, Options), A /= undefined
  ],
  StartAfter = [
    A || A <- proplists:get_all_values(start_after, Options), A /= undefined
  ],
  application:set_env(indira, start_before, StartBefore),
  application:set_env(indira, start_after, StartAfter),
  set_indira_options(Rest, Options);
set_indira_options([] = _Aspects, _Options) ->
  ok.

%% @doc Verify correctness of listener specifications.

-spec check_listen_specs(term()) ->
  ok | {error, invalid_listen_spec}.

check_listen_specs([_|_] = Specs) ->
  case lists:all(fun ({M,_}) -> is_atom(M); (_) -> false end, Specs) of
    true -> ok;
    false -> {error, invalid_listen_spec}
  end;
check_listen_specs(_Specs) ->
  {error, invalid_listen_spec}.

%% @doc Verify correctness of option values for Erlang networking.

-spec check_net_config(term(), term(), term()) ->
  ok | skip | {error, invalid_net_config}.

check_net_config(undefined = _NodeName, _NameType, _Cookie) ->
  skip;
check_net_config(_NodeName, undefined = _NameType, _Cookie) ->
  skip;
check_net_config(NodeName, NameType, undefined = _Cookie) ->
  check_net_config(NodeName, NameType, none);
check_net_config(NodeName, NameType, Cookie)
when (NameType == shortnames orelse NameType == longnames),
     is_atom(NodeName) ->
  case Cookie of
    %none -> % covered by `is_atom(Cookie)'
    %  ok;
    _ when is_atom(Cookie) ->
      ok;
    {file, CookieFile} when is_list(CookieFile) orelse is_binary(CookieFile) ->
      ok;
    _ ->
      {error, invalid_net_config}
  end;
check_net_config(_NodeName, _NameType, _Cookie) ->
  {error, invalid_net_config}.

%% }}}
%%----------------------------------------------------------

%% @doc Call reload function set with `{reload,{M,F,A}}' option.
%%
%% @see format_stacktrace/1

-spec reload() ->
  term() | {error, reload_not_set | reload_in_progress}.

reload() ->
  case application:get_env(indira, reload_function) of
    {ok, {Mod, Fun, Args}} ->
      case reload_mutex_lock() of
        true ->
          try
            apply(Mod, Fun, Args)
          after
            reload_mutex_unlock()
          end;
        false ->
          {error, reload_in_progress}
      end;
    undefined ->
      {error, reload_not_set}
  end.

%%----------------------------------------------------------
%% reload mutex {{{

%% @doc Try acquiring the mutex that guards reloading procedure.

-spec reload_mutex_lock() ->
  boolean().

reload_mutex_lock() ->
  Self = self(),
  Pid = spawn(fun() ->
    try register(?RELOAD_MUTEX_NAME, self()) of
      true ->
        Self ! {?RELOAD_MUTEX_NAME, self(), true},
        receive
          {unlock, Self} -> ok
        end
    catch
      error:badarg ->
        Self ! {?RELOAD_MUTEX_NAME, self(), false}
    end
  end),
  receive
    {?RELOAD_MUTEX_NAME, Pid, Result} -> Result
  end.

%% @doc Release the mutex that guards reloading procedure.

reload_mutex_unlock() ->
  ?RELOAD_MUTEX_NAME ! {unlock, self()}.

%% }}}
%%----------------------------------------------------------

%% @doc Format stacktrace returned by {@link erlang:get_stacktrace/0} as
%%   a JSON-serializable object.
%%
%% @see indira_json:encode/1

-spec format_stacktrace(StackTrace :: [Entry]) ->
  [indira_json:jhash()]
  when Entry :: {Module, Function, Args, Location},
       Module :: atom(),
       Function :: atom(),
       Args :: non_neg_integer() | [term()],
       Location :: [{atom(), term()}].

format_stacktrace(StackTrace) ->
  _Result = [
    [{function, format_function(M, F, A)} | format_location(L)] ||
    {M, F, A, L} <- StackTrace
  ].

%%----------------------------------------------------------
%% stacktrace formatting helpers {{{

%% @doc Format function name as a string (binary).

-spec format_function(atom(), atom(), integer() | list()) ->
  binary().

format_function(Mod, Fun, Args) when is_list(Args) ->
  format_function(Mod, Fun, length(Args));
format_function(Mod, Fun, Arity) when is_integer(Arity) ->
  iolist_to_binary([
    atom_to_list(Mod), $:,
    atom_to_list(Fun), $/,
    integer_to_list(Arity)
  ]).

%% @doc Format location as a JSON-serializable hash.

-spec format_location([{atom(), term()}]) ->
  indira_json:jhash().

format_location([] = _Location) ->
  [{}];
format_location(Location) ->
  [format_location_element(E) || E <- Location].

%% @doc Workhorse for {@link format_location/1}.

-spec format_location_element({atom(), term()}) ->
  {indira_json:jstring(), indira_json:jscalar()}.

format_location_element({file, File}) when is_list(File) ->
  {file, list_to_binary(File)};
format_location_element({line, Line}) when is_integer(Line) ->
  {line, Line};
format_location_element({Name, Value}) ->
  {Name, iolist_to_binary(io_lib:print(Value, 1, 16#FFFFFFFF, -1))}.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% starting applications
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% daemonize the main application {{{

%% @doc Start the main application of the daemon.
%%
%% Function sets all Indira's parameters specified in options (options
%% `{listen, [...]}' and `{command, {Mod,Arg}}' are mandatory), and then
%% starts `{start_before, _}' apps, Indira, `{start_after, _}' apps, and `App'
%% itself, in this order.
%%
%% Daemon may be configured in parts by using {@link indira_setup/1},
%% {@link set_env/4}, {@link set_option/3}, or {@link application:set_env/3}
%% called directly.
%%
%% Function never returns, causing the calling process to sleep forever.

-spec daemonize(atom(), [daemon_option()]) ->
  no_return() | {error, Reason}
  when Reason :: invalid_listen_spec | missing_listen_spec
               | invalid_command_handler | missing_command_handler
               | invalid_reload_function
               | invalid_pidfile
               | invalid_net_config | invalid_net_start.

daemonize(App, Options) ->
  case daemonize_set_options(Options) of
    ok ->
      {ok, StartBefore} = application:get_env(indira, start_before),
      {ok, StartAfter}  = application:get_env(indira, start_after),
      % TODO: handle errors
      ok = start_all_applications(StartBefore),
      ok = start_rec(indira),
      ok = start_all_applications(StartAfter),
      ok = start_rec(App),
      sleep_forever(); % never return
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Prepare options for {@link daemonize/2}.

-spec daemonize_set_options([daemon_option()]) ->
  ok | {error, Reason}
  when Reason :: invalid_listen_spec | missing_listen_spec
               | invalid_command_handler | missing_command_handler
               | invalid_reload_function
               | invalid_pidfile
               | invalid_net_config | invalid_net_start.

daemonize_set_options(Options) ->
  case indira_setup(Options) of
    ok ->
      % check if the mandatory options are correct
      ListenEnv  = application:get_env(indira, listen),
      CommandEnv = application:get_env(indira, command),
      case {ListenEnv, CommandEnv} of
        {{ok, [_ | _]}, {ok, {_, _}} } -> ok;
        {undefined, _} -> {error, missing_listen_spec};
        {{ok, []},  _} -> {error, missing_listen_spec};
        {_, undefined}       -> {error, missing_command_handler};
        {_, {ok, undefined}} -> {error, missing_command_handler}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Start all the specified applications.

-spec start_all_applications([atom()]) ->
  ok | {error, {App :: atom(), Reason :: term()}}.

start_all_applications([] = _Apps) ->
  ok;
start_all_applications([App | Rest] = _Apps) ->
  case start_rec(App) of
    ok -> start_all_applications(Rest);
    {error, Reason} -> {error, {App, Reason}}
  end.

%% }}}
%%----------------------------------------------------------
%% start application (recursively) {{{

%% @doc Start application along with all its dependencies.
%%
%%   NOTE: Application start type is `permanent', which is a different default
%%   than {@link application:start/1} has.
%%
%% @see application:start/2

-spec start_rec(atom()) ->
  ok | {error, term()}.

start_rec(App) ->
  % defaults to the same as application:start()
  start_rec(App, permanent).

%% @doc Start application along with all its dependencies.
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
%% check (or wait for) application start {{{

%% @doc Wait until the specified supervisor (and, transitively, all its
%%   children) starts properly.
%%
%%   On supervisor's start error, `error' is returned.

-spec wait_for_start(pid() | atom()) ->
  ok | error.

wait_for_start(Supervisor) ->
  try supervisor:which_children(Supervisor) of
    _ -> ok
  catch
    _:_ -> error
  end.

%% @doc Check if an application is started.

-spec is_started(atom()) ->
  boolean().

is_started(App) ->
  % `{AppName :: atom(), Desc :: string(), Version :: string()}' or `false';
  % only non-false when the application started successfully (it's still
  % `false' during boot time)
  AppEntry = lists:keyfind(App, 1, application:which_applications()),
  AppEntry /= false.

%% }}}
%%----------------------------------------------------------
%% sleep forever {{{

%% @doc Sleep forever.

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

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
