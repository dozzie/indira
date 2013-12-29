%-----------------------------------------------------------------------------

-module(indira_tcp_sup).

-behaviour(supervisor).

%-----------------------------------------------------------
% public API

% starting this supervisor
-export([start_link/3]).
% starting this supervisor as a workers pool (internal function)
-export([start_link_worker/1]).
% supervisor that acts as a workers pool (starting supervisor and its worker)
-export([start_worker_pool/2, new_worker/2]).

%-----------------------------------------------------------
% supervisor callbacks

-export([init/1]).

%-----------------------------------------------------------

%-----------------------------------------------------------------------------
% public API
%-----------------------------------------------------------------------------

%-----------------------------------------------------------
% starting supervisor process

start_link(CmdRecipient, Host, Port) ->
  supervisor:start_link(?MODULE, [acceptor, CmdRecipient, Host, Port]).

start_link_worker(CmdRecipient) ->
  supervisor:start_link(?MODULE, [worker, CmdRecipient]).

%-----------------------------------------------------------
% wrappers around `supervisor' module

start_worker_pool(Supervisor, CmdRecipient) ->
  Child = {
    indira_tcp_worker_pool_sup,
      {?MODULE, start_link_worker, [CmdRecipient]},
      permanent, 5000, supervisor, [?MODULE]
  },
  Result = supervisor:start_child(Supervisor, Child),
  io:fwrite("[] starting worker pool: ~p~n", [Result]),
  strip_info(Result).

new_worker(Supervisor, ClientSocket) ->
  io:fwrite("[] starting child in ~p~n", [Supervisor]),
  strip_info(supervisor:start_child(Supervisor, [ClientSocket])).


% strip `Info' field from `{ok, Child, Info}' tuple, to always return
% `{ok, Child}' on success
strip_info({ok, _Child} = Result) ->
  Result;
strip_info({ok, Child, _Info}) ->
  {ok, Child};
strip_info(Any) ->
  Any.

%-----------------------------------------------------------------------------
% supervisor callbacks
%-----------------------------------------------------------------------------

init([acceptor, CmdRecipient, Host, Port] = _Args) ->
  io:fwrite("[indira TCP sup] self() = ~p~n", [self()]),
  Strategy = {one_for_all, 5, 10},
  Children = [
    {indira_tcp,
      {indira_tcp, start_link, [self(), CmdRecipient, Host, Port]},
      permanent, 5000, worker, [indira_tcp]}
  ],
  {ok, {Strategy, Children}};

init([worker, CmdRecipient] = _Args) ->
  io:fwrite("[indira TCP sup worker] self() = ~p~n", [self()]),
  Strategy = {simple_one_for_one, 5, 10},
  Children = [
    {undefined,
      {indira_tcp, start_link_worker, [CmdRecipient]},
      temporary, 5000, worker, [indira_tcp_client]}
  ],
  {ok, {Strategy, Children}}.

%-----------------------------------------------------------------------------
% vim:ft=erlang:foldmethod=marker
