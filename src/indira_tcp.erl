%-----------------------------------------------------------------------------
% TCP listener
%-----------------------------------------------------------------------------

-module(indira_tcp).

-behaviour(gen_server).

% public API for supervision tree
-export([start_link/1]).

% gen_server API
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%-----------------------------------------------------------------------------

-include_lib("kernel/include/inet.hrl").

-record(state, {socket, listener, child_sup}).

%-----------------------------------------------------------------------------
% public API
%-----------------------------------------------------------------------------

% nuffin yet

%-----------------------------------------------------------------------------
% public API for supervision tree
%-----------------------------------------------------------------------------

start_link(ListenSpec) ->
  io:fwrite("[indira TCP] starting: ~p~n", [ListenSpec]),
  gen_server:start_link(?MODULE, ListenSpec, []).

%-----------------------------------------------------------------------------
% gen_server API
%-----------------------------------------------------------------------------

init({tcp = _Protocol, Address, Port} = _ListenSpec) ->
  io:fwrite("[indira TCP] self() = ~p~n", [self()]),
  % create listening socket
  BindOpt = address_to_bind_option(Address),
  {ok, Sock} = gen_tcp:listen(Port, BindOpt ++ [
    {active, false}, {reuseaddr, true}, list, {packet, line}
  ]),

  {ok, ChildSupervisor} = indira_tcp_sup:start_link(accepted),

  % start acceptor (infinite loop)
  Parent = self(),
  Pid = spawn_link(fun() ->
    acceptor_loop(Sock, Parent, ChildSupervisor)
  end),

  State = #state{
    socket    = Sock,
    listener  = Pid,
    child_sup = ChildSupervisor
  },
  {ok, State}.

terminate(normal, State) ->
  terminate(shutdown, State);
terminate(Reason, State) ->
  io:fwrite("[indira TCP] stopping (~p)~n", [Reason]),
  unlink(State#state.listener),
  exit(State#state.listener, Reason),
  unlink(State#state.child_sup),
  exit(State#state.child_sup, Reason),
  gen_tcp:close(State#state.socket),
  ok.

handle_call(Request, _From, State) ->
  case Request of
    stop ->
      io:fwrite("[indira TCP] stopping~n"),
      {stop, normal, ok, State};
    %{'EXIT', _Pid, shutdown} ->
    %  io:fwrite("[indira stopping on shutdown]~n"),
    %  {stop, shutdown, ok, State};
    _Any ->
      io:fwrite("[indira TCP] call: ~p~n", [_Any]),
      {reply, ok, State}
  end.

handle_cast(_Request, State) ->
  io:fwrite("[indira TCP] cast: ~p~n", [_Request]),
  {noreply, State}.

handle_info(Message, State) ->
  case Message of
    {command, Command} ->
      io:fwrite("[indira TCP] got command: ~p~n", [Command]),
      {noreply, State};
    _Any ->
      io:fwrite("[indira TCP] info: ~p~n", [_Any]),
      {noreply, State}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%-----------------------------------------------------------------------------
% networking internals
%-----------------------------------------------------------------------------

address_to_bind_option(any) ->
  [];
address_to_bind_option(Addr) when is_list(Addr) ->
  {ok, #hostent{h_addr_list = HostAddrs}} = inet:gethostbyname(Addr),
  [HostAddr | _Rest] = HostAddrs,
  [{ip, HostAddr}];
address_to_bind_option(Addr) when is_tuple(Addr) ->
  [{ip, Addr}].

%-----------------------------------------------------------------------------

acceptor_loop(Sock, Parent, ChildSupervisor) ->
  case gen_tcp:accept(Sock) of
    {ok, Client} ->
      io:fwrite("[indira TCP acceptor] spawning new client handler~n"),
      {ok, Worker} = indira_tcp_sup:new_client_process(
        ChildSupervisor, [Client, Parent, self()]
      ),
      io:fwrite("[indira TCP acceptor] worker is at ~p~n", [Worker]),
      gen_tcp:controlling_process(Client, Worker),
      inet:setopts(Client, [{active, true}]),
      acceptor_loop(Sock, Parent, ChildSupervisor);
    {error, _Reason} ->
      % TODO: log this
      acceptor_loop(Sock, Parent, ChildSupervisor)
  end.

%-----------------------------------------------------------------------------
% vim:ft=erlang:foldmethod=marker
