%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Admin sockets supervisor.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_socket_sup).

-behaviour(supervisor).

%% public API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start the supervisor process.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @private
%% @doc Initialize supervisor.

init(_Args) ->
  Strategy = {one_for_one, 5, 10},
  ConnWorkersSupervisors = [
    {indira_tcp_conn_sup,
      {indira_tcp_conn_sup, start_link, []},
      permanent, 5000, supervisor, [indira_tcp_conn_sup]},
    {indira_unix_conn_sup,
      {indira_unix_conn_sup, start_link, []},
      permanent, 5000, supervisor, [indira_unix_conn_sup]}
  ],
  {ok, ListenSpecs} = application:get_env(listen),
  SocketWorkers = [child_spec(Mod, Addr) || {Mod, Addr} <- ListenSpecs],
  Children = ConnWorkersSupervisors ++ SocketWorkers,
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------

%% @doc Make a child specification from a listen specification.

-spec child_spec(module(), term()) ->
  supervisor:child_spec().

child_spec(Module, Addr) when is_atom(Module) ->
  {_Id, MFA, _Restart, Shutdown, Type, Modules} = Module:child_spec(Addr),
  {{Module, Addr}, MFA, permanent, Shutdown, Type, Modules}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
