%%%---------------------------------------------------------------------------
%%%
%%% Indira TCP listener supervisor.
%%% TCP workers supervisor (worker pool).
%%%
%%% NOTE: Yes, this module serves two purposes, depending on call options.
%%%
%%%---------------------------------------------------------------------------

-module(indira_tcp_sup).

-behaviour(supervisor).

%% acceptors supervisor
-export([start_link/3]).        % start the supervisor
-export([start_worker_pool/1]). % start its child (i.e., workers supervisor)

%% workers supervisor
-export([start_link_worker/1]). % start the supervisor
-export([new_worker/2]).        % start its child (i.e., worker)

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% starting supervisor process

start_link(CmdRecipient, Host, Port) ->
  supervisor:start_link(?MODULE, [acceptor, CmdRecipient, Host, Port]).

start_link_worker(CmdRecipient) ->
  supervisor:start_link(?MODULE, [worker, CmdRecipient]).

%%----------------------------------------------------------
%% wrappers around `supervisor' module

start_worker_pool(Supervisor) ->
  % FIXME: this is subject to a race condition with parent
  % NOTE: this is somewhat ugly to manually search through the children, but
  % I have little better alternatives on how this should work actually
  Children = supervisor:which_children(Supervisor),
  {_, Pid, _, _} = lists:keyfind(indira_tcp_worker_pool_sup, 1, Children),
  {ok, Pid}.

new_worker(Supervisor, ClientSocket) ->
  strip_info(supervisor:start_child(Supervisor, [ClientSocket])).


%% strip `Info' field from `{ok, Child, Info}' tuple, to always return
%% `{ok, Child}' on success
strip_info({ok, _Child} = Result) ->
  Result;
strip_info({ok, Child, _Info}) ->
  {ok, Child};
strip_info(Any) ->
  Any.

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

init([acceptor, CmdRecipient, Host, Port] = _Args) ->
  Strategy = {one_for_all, 5, 10},
  Children = [
    {indira_tcp,
      {indira_tcp, start_link, [self(), Host, Port]},
      permanent, 5000, worker, [indira_tcp]},
    {indira_tcp_worker_pool_sup,
      {?MODULE, start_link_worker, [CmdRecipient]},
      permanent, 5000, supervisor, [?MODULE]}
  ],
  {ok, {Strategy, Children}};

init([worker, CmdRecipient] = _Args) ->
  Strategy = {simple_one_for_one, 5, 10},
  Children = [
    {undefined,
      {indira_tcp, start_link_worker, [CmdRecipient]},
      temporary, 5000, worker, [indira_tcp_client]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
