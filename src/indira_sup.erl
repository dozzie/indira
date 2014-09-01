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

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%% @doc Start the supervisor process.
start_link() ->
  supervisor:start_link(?MODULE, []).

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize supervisor.
init([] = _Args) ->
  Strategy = {one_for_all, 5, 10},
  Children = [
    {indira_router,
      {indira_router, start_link, []},
      permanent, 5000, worker, [indira_router]},
    {indira_listener_sup,
      {indira_listener_sup, start_link, []},
      permanent, 5000, supervisor, [indira_listener_sup]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
