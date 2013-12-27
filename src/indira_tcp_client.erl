%-----------------------------------------------------------------------------

-module(indira_tcp_client).

-behaviour(gen_fsm).

%-----------------------------------------------------------
% public API

-export([start/1, start_link/1]).

%-----------------------------------------------------------
% gen_fsm callbacks

% generic ones
-export([init/1, terminate/3]).
-export([handle_event/3, handle_sync_event/4]).
-export([handle_info/3]).
-export([code_change/4]).

% states
-export(['READ_COMMAND'/2, 'READ_COMMAND'/3]).

%-----------------------------------------------------------

% socket -- socket to read from
% server -- PID to send commands to
-record(data, {socket, server}).

%-----------------------------------------------------------

%-----------------------------------------------------------------------------
% public API
%-----------------------------------------------------------------------------

start_link(Args) ->
  io:fwrite("[indira FSM]: starting linked~n"),
  gen_fsm:start_link(?MODULE, Args, []).

start(Args) ->
  io:fwrite("[indira FSM]: starting~n"),
  gen_fsm:start(?MODULE, Args, []).

%-----------------------------------------------------------------------------
% gen_fsm generic callbacks
%-----------------------------------------------------------------------------

init([Socket, ServerPid, _Acceptor] = Args) ->
  io:fwrite("[indira FSM] init: ~p~n", [Args]),
  io:fwrite("[indira FSM] self() = ~p~n", [self()]),
  Data = #data{socket = Socket, server = ServerPid},
  {ok, 'READ_COMMAND', Data}.

terminate(Reason, _StateName, _StateData) ->
  io:fwrite("[indira FSM] terminated: ~p~n", [Reason]),
  ok.

% gen_fsm:send_all_state_event/2
handle_event(_Event, StateName, StateData) ->
  io:fwrite("[indira FSM] async event (all states)~n"),
  {next_state, StateName, StateData}.

% gen_fsm:sync_send_all_state_event/2,3
handle_sync_event(_Event, _From, StateName, StateData) ->
  io:fwrite("[indira FSM] sync event (all states)~n"),
  {reply, ok, StateName, StateData}.

% message, possibly a network packet
handle_info({tcp_closed, Socket} = _Info, _StateName, StateData) ->
  io:fwrite("[indira FSM] client disconnected (~p)~n", [Socket]),
  gen_tcp:close(Socket),
  {stop, normal, StateData#data{socket = undefined}};
handle_info({tcp, Socket, Line} = Info,
            StateName, StateData = #data{socket = Socket}) ->
  io:fwrite("[indira FSM] got command: ~p~n", [Info]),
  % TODO: use gen_server:call() or gen_server:cast()
  StateData#data.server ! {command, Line},
  {next_state, StateName, StateData};
handle_info(Info, StateName, StateData) ->
  io:fwrite("[indira FSM] unknown message: ~p~n", [Info]),
  {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%-----------------------------------------------------------------------------
% gen_fsm state callbacks
%-----------------------------------------------------------------------------

% Foo(Event, StateData) -> Result.
%   Event -- 'timeout' or an argument to gen_fsm:send_event/2
%   StateData -- #data{}
%   Result:
%     {next_state, Name, #data{...}}
%     {next_state, Name, #data{...}, Timeout}
%     {stop, Reason, #data{...}}

% Foo(Event, From, StateData) -> Result.
%   Event -- 'timeout' or an argument to gen_fsm:send_event/2
%   From  -- for use with gen_fsm:reply/2
%   StateData -- #data{}
%   Result (last three require manual use of gen_fsm:reply/2):
%     {reply, Reply, Name, #data{...}}
%     {reply, Reply, Name, #data{...}, Timeout}
%     {stop, Reply, Reason, #data{...}}
%     {next_state, Name, #data{...}}
%     {next_state, Name, #data{...}, Timeout}
%     {stop, Reason, #data{...}}

'READ_COMMAND'(_Event, StateData) ->
  io:fwrite("[indira FSM] async event~n"),
  {next_state, 'READ_COMMAND', StateData}.

'READ_COMMAND'(_Event, _From, StateData) ->
  io:fwrite("[indira FSM] sync event~n"),
  {reply, ok, 'READ_COMMAND', StateData}.

%-----------------------------------------------------------------------------
% vim:ft=erlang:foldmethod=marker
