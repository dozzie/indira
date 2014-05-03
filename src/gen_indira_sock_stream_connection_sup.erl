%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Supervisor that watches over connection handler processes.
%%%
%%% @TODO Possibility of spawning workers running different modules.
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_sock_stream_connection_sup).

-behaviour(supervisor).

-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%% @doc Start the supervisor process.
%%
%%   `ConnHandler' is expected to have function name explicit.
%%
%% @spec start_link(gen_indira_sock_stream:connection_handler()) ->
%%   {ok, pid()} | {error, Reason}

start_link(ConnHandler) ->
  supervisor:start_link(?MODULE, ConnHandler).

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize supervisor.

init({Module, Function} = _ConnHandler) ->
  Strategy = {simple_one_for_one, 5, 10},
  Children = [
    {undefined,
      {Module, Function, []},
      temporary, 5000, worker, [Module]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
