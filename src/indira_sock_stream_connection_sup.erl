%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Supervisor that watches over connection handler processes.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_sock_stream_connection_sup).

-behaviour(supervisor).

-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start the supervisor process.
%%
%%   `ConnHandler' is expected to have function name explicit.

-spec start_link(gen_indira_sock_stream:connection_handler()) ->
  {ok, pid()} | {error, term()}.

start_link(ConnHandler) ->
  supervisor:start_link(?MODULE, ConnHandler).

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @private
%% @doc Initialize supervisor.

init({Module, Function} = _ConnHandler) ->
  put(indira_stream_handler, {Module, Function}),
  Strategy = {simple_one_for_one, 5, 10},
  Children = [
    {undefined,
      {Module, Function, []},
      temporary, 5000, worker, [Module]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
