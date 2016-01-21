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
  Strategy = {one_for_all, 5, 10},
  Children = [
    {indira_pidfile,
      {indira_pidfile, start_link, []},
      permanent, 5000, worker, [indira_pidfile]},
    {indira_dist_erl,
      {indira_dist_erl, start_link, []},
      permanent, 5000, worker, [indira_dist_erl]},
    {indira_command_sup,
      {indira_command_sup, start_link, []},
      permanent, 5000, supervisor, [indira_command_sup]},
    {indira_commander,
      {indira_commander, start_link, []},
      permanent, 5000, worker, [indira_commander]},
    {indira_listener_sup,
      {indira_listener_sup, start_link, []},
      permanent, 5000, supervisor, [indira_listener_sup]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
