%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Indira top-level supervisor.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_sup).

-behaviour(supervisor).

%% public API
-export([start_link/0]).
-export([start_listener_pool/1, start_listener/2]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%% @doc Start the supervisor process.
start_link() ->
  supervisor:start_link(?MODULE, []).

%%%---------------------------------------------------------------------------

%% @doc Start listeners supervisor.
%%
%% @spec start_listener_pool(pid()) ->
%%   {ok, Pid} | {error, Reason}

start_listener_pool(Supervisor) ->
  % FIXME: this is subject to a race condition with parent
  % NOTE: this is somewhat ugly to manually search through the children, but
  % I have little better alternatives on how this should work actually
  Children = supervisor:which_children(Supervisor),
  {_, Pid, _, _} = lists:keyfind(indira_listener_sup, 1, Children),
  {ok, Pid}.

%% @doc Start new listener child.
%%   The child can be a worker or a supervisor, according to
%%   {@link gen_indira_listener}.
start_listener(ListenerSupervisor, ListenerSpec) ->
  indira_listener_sup:start_listener(ListenerSupervisor, ListenerSpec).

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize supervisor.
init([] = _Args) ->
  Strategy = {one_for_all, 5, 10},
  Children = [
    {indira_router,
      {indira_router, start_link, [self()]},
      permanent, 5000, worker, [indira_router]},
    {indira_listener_sup,
      {indira_listener_sup, start_link, []},
      permanent, 5000, supervisor, [indira_listener_sup]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
