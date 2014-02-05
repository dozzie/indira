%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira TCP listener supervisor.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_tcp_sup).

-behaviour(supervisor).

-export([start_link/3]).

%% workers supervisor
-export([start_worker_pool/1]). % start workers supervisor)
-export([start_worker/2]).      % start worker

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% starting supervisor process

%% @doc Start the supervisor process.
start_link(CmdRouter, Host, Port) ->
  supervisor:start_link(?MODULE, {CmdRouter, Host, Port}).

%%----------------------------------------------------------
%% wrappers around `supervisor' module

%% @doc Start TCP readers supervisor.
%%
%% @spec start_worker_pool(pid()) ->
%%   {ok, Pid} | {error, Reason}

start_worker_pool(Supervisor) ->
  % FIXME: this is subject to a race condition with parent
  % NOTE: this is somewhat ugly to manually search through the children, but
  % I have little better alternatives on how this should work actually
  Children = supervisor:which_children(Supervisor),
  {_, Pid, _, _} = lists:keyfind(indira_tcp_worker_pool_sup, 1, Children),
  {ok, Pid}.

%% @doc Start new reader child for a given TCP socket.
%% @see indira_tcp_reader_sup:start_worker/2
start_worker(Supervisor, ClientSocket) ->
  indira_tcp_reader_sup:start_worker(Supervisor, ClientSocket).

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize supervisor.
init({CmdRouter, Host, Port} = _Args) ->
  Strategy = {one_for_all, 5, 10},
  Children = [
    {indira_tcp_listener,
      {indira_tcp_listener, start_link, [self(), Host, Port]},
      permanent, 5000, worker, [indira_tcp_listener]},
    {indira_tcp_worker_pool_sup,
      {indira_tcp_reader_sup, start_link, [CmdRouter]},
      permanent, 5000, supervisor, [indira_tcp_reader_sup]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
