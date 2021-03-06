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
  Strategy = {one_for_one, 5, 10},
  Children = [
    {indira_boot_sup,
      {indira_boot_sup, start_link, []},
      permanent, 5000, supervisor, [indira_boot_sup]},
    {indira_command_sup,
      {indira_command_sup, start_link, []},
      permanent, 5000, supervisor, [indira_command_sup]},
    {indira_socket_sup,
      {indira_socket_sup, start_link, []},
      permanent, 5000, supervisor, [indira_socket_sup]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
