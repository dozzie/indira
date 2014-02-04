%%%---------------------------------------------------------------------------
%%%
%%% TCP workers supervisor (worker pool).
%%%
%%%---------------------------------------------------------------------------

-module(indira_tcp_reader_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([new_worker/2]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% starting supervisor process

start_link(CmdRecipient) ->
  supervisor:start_link(?MODULE, CmdRecipient).

new_worker(Supervisor, ClientSocket) ->
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

init(CmdRecipient) ->
  Strategy = {simple_one_for_one, 5, 10},
  Children = [
    {undefined,
      {indira_tcp_reader, start_link, [CmdRecipient]},
      temporary, 5000, worker, [indira_tcp_reader]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
