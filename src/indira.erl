%-----------------------------------------------------------------------------

-module(indira).

-behaviour(gen_server).

% public API for supervision tree
-export([start_link/1]).
-export([start/0]).

% gen_server API
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%-----------------------------------------------------------------------------

-include_lib("kernel/include/inet.hrl").

-record(state, {child_sup}).

%-----------------------------------------------------------------------------
% public API
%-----------------------------------------------------------------------------

% nuffin yet

%-----------------------------------------------------------------------------
% public API for supervision tree
%-----------------------------------------------------------------------------

start_link(ListenSpec) ->
  io:fwrite("indira: ~p~n", [ListenSpec]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, ListenSpec, []).

% convenience wrapper
start() ->
  application:start(indira, transient).

%-----------------------------------------------------------------------------
% gen_server API
%-----------------------------------------------------------------------------

init(ListenSpec) ->
  {ok, Supervisor} = indira_tcp_sup:start_link(listening),
  [indira_tcp_sup:new_client_process(Supervisor, Spec) || Spec <- ListenSpec],
  State = #state{child_sup = Supervisor},
  {ok, State}.

terminate(normal, State) ->
  terminate(shutdown, State);
terminate(Reason, State) ->
  io:fwrite("[indira stopping] unlinking children~n"),
  unlink(State#state.child_sup),
  io:fwrite("[indira stopping] stopping children~n"),
  exit(State#state.child_sup, Reason),
  ok.

handle_call(Request, _From, State) ->
  case Request of
    stop ->
      io:fwrite("[indira stopping]~n"),
      {stop, normal, ok, State};
    _Any ->
      io:fwrite("#indira# ~p~n", [_Any]),
      {reply, ok, State}
  end.

handle_cast(_Request, State) ->
  io:fwrite("#indira# ~p~n", [_Request]),
  {noreply, State}.

handle_info(Message, State) ->
  case Message of
    {command, Command} ->
      io:fwrite("[indira] ~p~n", [Command]),
      {noreply, State};
    _Any ->
      io:fwrite("#indira# ~p~n", [_Any]),
      {noreply, State}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%-----------------------------------------------------------------------------
% vim:ft=erlang:foldmethod=marker
