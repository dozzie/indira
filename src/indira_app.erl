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
-export([set_option/3, set_env/4, set_env/3, indira_setup/1]).
%% starting applications
-export([start_rec/1, start_rec/2, daemonize/2]).
-export([sleep_forever/0]).
%% starting/stopping distributed Erlang
-export([distributed_start/0, distributed_stop/0]).

-export_type([daemon_option/0]).
-export_type([set_spec/0, set_option/0]).
-export_type([config/0, config_key/0, config_value/0, env_key/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

-type daemon_option() ::
    {listen, [{module(), gen_indira_socket:listen_address()}]}
  | {command, {module(), term()}}
  | {pidfile, file:filename() | undefined}
  | {node_name, node() | undefined}
  | {name_type, shortnames | longnames | undefined}
  | {cookie, none | undefined | atom() | {file, file:filename()}}
  | {net_start, boolean() | undefined}
  | {start_before, AppName :: atom()}
  | {start_after, AppName :: atom()}.
%% Options that correspond to Indira's environment. `node_name', `name_type',
%% and `cookie' compose <i>indira/net</i> parameter. See {@link configuration}
%% for details. `{start_*, AppName}' allow to start additional applications
%% (e.g. SASL) before or after Indira.

-type config() :: term().
%% Structure with configuration options loaded from a file.

-type config_key() :: term().
%% Option name to lookup in {@type config()}.

-type config_value() :: term().
%% Value of {@type config_key()} in {@type config()}.

-type env_key() :: {AppName :: atom(), Parameter :: atom()}.
%% Application environment parameter name.

-type set_spec() ::
    {config_key(), env_key()}
  | {config_key(), env_key(), [set_option()]}.
%% Specification for setting an app parameter.

-type set_option() :: append | if_set | if_unset.
%% When and how to set an app parameter.

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

%%%---------------------------------------------------------------------------
%%% setting options
%%%---------------------------------------------------------------------------

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

%% @doc Set Indira application's environment parameters.
%%
%% NOTE: Setting an option to `undefined' has the same result as omitting it
%% altogether.

-spec indira_setup([daemon_option()]) ->
  ok | {error, Reason}
  when Reason :: invalid_listen_spec
               | invalid_command_handler
               | invalid_pidfile
               | invalid_net_config
               | invalid_net_start.

indira_setup(Options) ->
  % TODO: handle error
  case application:load(indira) of
    ok -> ok;
    {error, {already_loaded, indira}} -> ok
  end,
  set_indira_options([listen, command, pidfile, net, net_start, apps], Options).

%%----------------------------------------------------------
%% validate and set Indira options {{{

%% @doc Workhorse for {@link indira_setup/1}.

-spec set_indira_options([atom()], [daemon_option()]) ->
  ok | {error, Reason}
  when Reason :: invalid_listen_spec
               | invalid_command_handler
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
  Cookie = proplists:get_value(cookie, Options, none),
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

%% @doc Set application environment from config according to specification.
%%
%%   `Config' is a configuration structure, possibly loaded
%%   from a file (e.g. a proplist).
%%
%%   `ConfigGet' is a function that extracts a single configuration value from
%%   the structure (e.g. {@link proplists:get_value/2}); it's called as
%%   `ConfigGet(Key, Config)', `Key' being a part of set spec.
%%
%%   `Validate' takes the value returned by `ConfigGet(...)', checks it for
%%   correctness and possibly converts to an appropriate type (e.g. from
%%   {@type binary()} to {@type @{inet:hostname(), inet:port_number()@}}).
%%   It's called as `Validate(Key, EnvKey, Value)', `Key' and `EnvKey' being
%%   parts of set spec and `Value' being a value returned by `ConfigGet()'.
%%
%%   `SetSpecs' is a list of tuples describing what value from config to use
%%   to set what application's environment. Single specification takes form of
%%   `{Key, EnvKey, SetOpts}' or `{Key, EnvKey}'. `Key' is what is passed to
%%   `ConfigGet()' along with configuration. `EnvKey' is a pair of two atoms,
%%   `{Application,Par}', which are used with {@link application:get_env/2}
%%   and {@link application:set_env/3}.
%%
%%   Function pre-loads all the (potentially) necessary applications.
%%
%%   If `Validate(...)' returns `{error, Reason}', it will be reported as
%%   `{error, {Key, EnvKey, Reason}}'.

-spec set_env(ConfigGet, Validate, config(), [set_spec()]) ->
  ok | {error, {config_key(), env_key(), term()} | {app_load, term()}}
  when
    ConfigGet :: fun((config_key(), config()) ->
                       config_value()),
    Validate  :: fun((config_key(), env_key(), config_value()) ->
                       ok | {ok, NewValue :: term()} |
                       ignore | {error, term()}).

set_env(ConfigGet, Validate, Config, SetSpecs) ->
  case load_apps(SetSpecs) of
    ok -> set_env_loop(ConfigGet, Validate, Config, SetSpecs);
    {error, Reason} -> {error, {app_load, Reason}}
  end.

%% @doc Set application environment from config according to specification.
%%
%%   Simplified version of {@link set_env/4}, with `ConfigGet' set to {@link
%%   proplists:get_value/2}. This requires `Config' to be a proplist,
%%   obviously.

set_env(Validate, Config, SetSpecs) ->
  set_env(fun proplists:get_value/2, Validate, Config, SetSpecs).

%%----------------------------------------------------------
%% set_env_loop() {{{

%% @doc Set app environment, worker for {@link set_env/4}.

set_env_loop(_ConfigGet, _Validate, _Config, [] = _SetSpecs) ->
  ok;
set_env_loop(ConfigGet, Validate, Config, [Spec | RestSpecs] = _SetSpecs) ->
  case Spec of
    {Key, EnvKey, SetOpts} -> ok;
    {Key, EnvKey} -> SetOpts = []
  end,
  Value = ConfigGet(Key, Config),
  case Validate(Key, EnvKey, Value) of
    ignore ->
      set_env_loop(ConfigGet, Validate, Config, RestSpecs);
    ok ->
      set_app_env(EnvKey, Value, SetOpts),
      set_env_loop(ConfigGet, Validate, Config, RestSpecs);
    {ok, EnvValue} ->
      set_app_env(EnvKey, EnvValue, SetOpts),
      set_env_loop(ConfigGet, Validate, Config, RestSpecs);
    {error, Reason} ->
      {error, {Key, EnvKey, Reason}}
  end.

%% }}}
%%----------------------------------------------------------
%% set_app_env() {{{

%% @doc Set an application parameter to specified value.

-spec set_app_env(env_key(), term(), [set_option()]) ->
  ok.

set_app_env({App, Param} = _EnvKey, Value, Options) ->
  case should_set(App, Param, Options) of
    true ->
      case should_append(Options) of
        true ->
          CurrentValue = case application:get_env(App, Param) of
            {ok, V} -> V;
            undefined -> []
          end,
          application:set_env(App, Param, CurrentValue ++ [Value]);
        false ->
          application:set_env(App, Param, Value)
      end;
    false ->
      ok
  end.

%% @doc Determine whether an option should be set or not, according to
%%   options.

-spec should_set(atom(), atom(), [set_option()]) ->
  boolean().

should_set(App, Param, Options) ->
  Set = case application:get_env(App, Param) of
    {ok, _} -> set;
    undefined -> unset
  end,
  IfSet   = proplists:get_bool(if_set, Options),
  IfUnset = proplists:get_bool(if_unset, Options),
  case {IfSet, IfUnset, Set} of
    {true, _, set}    -> true;
    {_, true, unset}  -> true;
    {false, false, _} -> true; % if neither specified, set always
    {_, _, _}         -> false
  end.

%% @doc Determine whether an option should be appended to a current value.

-spec should_append([set_option()]) ->
  boolean().

should_append(Options) ->
  proplists:get_bool(append, Options).

%% }}}
%%----------------------------------------------------------
%% load_apps() {{{

%% @doc Load applications that will be potentially used in `SetSpecs'.

-spec load_apps([set_spec()]) ->
  ok | {error, term()}.

load_apps([] = _SetSpecs) ->
  ok;
load_apps([{_Key, {App, _Par}} | RestSpecs] = _SetSpecs) ->
  case application:load(App) of
    ok -> load_apps(RestSpecs);
    {error, {already_loaded, App}} -> load_apps(RestSpecs);
    {error, Reason} -> {error, Reason}
  end;
load_apps([{_Key, {App, _Par}, _Opts} | RestSpecs] = _SetSpecs) ->
  case application:load(App) of
    ok -> load_apps(RestSpecs);
    {error, {already_loaded, App}} -> load_apps(RestSpecs);
    {error, Reason} -> {error, Reason}
  end;
load_apps(_SetSpecs) ->
  {error, badarg}.

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
