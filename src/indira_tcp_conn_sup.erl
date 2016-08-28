%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   TCP connection handlers supervisor.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_tcp_conn_sup).

-behaviour(supervisor).

%% public interface
-export([spawn_worker/1]).

%% supervision tree API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Start a new worker process.

spawn_worker(Socket) ->
  supervisor:start_child(?MODULE, [Socket]).

%%%---------------------------------------------------------------------------
%%% supervision tree API
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
  Strategy = {simple_one_for_one, 5, 10},
  Children = [
    {undefined,
      {indira_tcp_conn, start_link, []},
      temporary, 5000, worker, [indira_tcp_conn]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
