%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Daemon boot operations supervisor.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_boot_sup).

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
  Children = [
    {indira_chdir,
      {indira_chdir, start_link, []},
      temporary, 5000, worker, [indira_chdir]},
    {indira_pidfile,
      {indira_pidfile, start_link, []},
      permanent, 5000, worker, [indira_pidfile]},
    {indira_dist_erl,
      {indira_dist_erl, start_link, []},
      permanent, 5000, worker, [indira_dist_erl]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
