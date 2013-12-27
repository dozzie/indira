%-----------------------------------------------------------------------------

-module(indira_tcp_sup).

-behaviour(supervisor).

%-----------------------------------------------------------
% public API

-export([start_link/1]).
-export([new_client_process/2]).

%-----------------------------------------------------------
% supervisor callbacks

-export([init/1]).

%-----------------------------------------------------------

%-----------------------------------------------------------------------------
% public API
%-----------------------------------------------------------------------------

-spec start_link(listening | accepted) ->
  {ok, pid()} | {error, Reason :: term()}.

start_link(Type) ->
  io:fwrite("indira sup ~p: starting~n", [self()]),
  supervisor:start_link(?MODULE, Type).

-spec new_client_process(pid(), [term()]) -> {ok, pid()}.

new_client_process(Supervisor, Args) ->
  case supervisor:start_child(Supervisor, [Args]) of
    {ok, Child}        -> {ok, Child};
    {ok, Child, _Info} -> {ok, Child};
    Error -> Error
  end.

%-----------------------------------------------------------------------------
% supervisor callbacks
%-----------------------------------------------------------------------------

-spec init(listening | accepted) ->
  {ok, {Strategy :: term(), Children :: term()}}.

init(listening) ->
  Strategy = {simple_one_for_one, 5, 10},
  Children = [
    {undefined,
      {indira_tcp, start_link, []},
      transient, 5000, worker, [indira_tcp]}
  ],
  {ok, {Strategy, Children}};

init(accepted) ->
  Strategy = {simple_one_for_one, 5, 10},
  Children = [
    {undefined,
      {indira_tcp_client, start_link, []},
      temporary, 5000, worker, [indira_tcp_client]}
  ],
  {ok, {Strategy, Children}}.

%-----------------------------------------------------------------------------
% vim:ft=erlang:foldmethod=marker
