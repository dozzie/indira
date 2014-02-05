%%%---------------------------------------------------------------------------
%%% @doc
%%%   TCP workers supervisor (worker pool).
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_tcp_reader_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([start_worker/2]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% starting supervisor process

%% @doc Start the supervisor process.
start_link(CmdRouter) ->
  supervisor:start_link(?MODULE, CmdRouter).

%% @doc Start new worker child for a socket.
start_worker(Supervisor, ClientSocket) ->
  % strip `Info' field from `{ok, Child, Info}' tuple, to always return
  % `{ok, Child}' on success
  case supervisor:start_child(Supervisor, [ClientSocket]) of
    {ok, _Child} = Result ->
      Result;
    {ok, Child, _Info} ->
      {ok, Child};
    Error ->
      Error
  end.

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize supervisor.
init(CmdRouter) ->
  Strategy = {simple_one_for_one, 5, 10},
  Children = [
    {undefined,
      {indira_tcp_reader, start_link, [CmdRouter]},
      temporary, 5000, worker, [indira_tcp_reader]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
