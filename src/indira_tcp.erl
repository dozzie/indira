%-----------------------------------------------------------------------------
% TCP listener
%-----------------------------------------------------------------------------

-module(indira_tcp).

-behaviour(gen_server).

% public API for supervision tree
-export([start_link/1]).
-export([start_link_client/1]).

% gen_server API
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%-----------------------------------------------------------------------------

-include_lib("kernel/include/inet.hrl").

-record(state, {socket, listener, child_sup, command_recipient}).

%-----------------------------------------------------------------------------
% public API for supervision tree
%-----------------------------------------------------------------------------

% spawn process that listens on TCP socket, accepts connections and spawns
% reader workers
start_link(ListenSpec) ->
  io:fwrite("[indira TCP] starting: ~p~n", [ListenSpec]),
  gen_server:start_link(?MODULE, ListenSpec, []).

% spawn process that reads everything from TCP socket
% (not a part of gen_server; probably should get extracted)
start_link_client([Socket, CmdRecipient, Parent]) ->
  Pid = spawn_link(fun() ->
    reader_loop(Socket, CmdRecipient, Parent)
  end),
  {ok, Pid}.

%-----------------------------------------------------------------------------
% gen_server API
%-----------------------------------------------------------------------------

init({CmdRecipient, {tcp = _Protocol, Address, Port} = _ListenSpec}) ->
  io:fwrite("[indira TCP] self() = ~p~n", [self()]),
  % create listening socket
  BindOpt = address_to_bind_option(Address),
  {ok, Socket} = gen_tcp:listen(Port, BindOpt ++ [
    {active, false}, {reuseaddr, true}, list, {packet, line}
  ]),

  {ok, ChildSupervisor} = indira_tcp_sup:start_link(accepted),

  % start acceptor (infinite loop) in a separate, linked process
  Listener = spawn_link(fun() ->
    acceptor_loop(Socket, CmdRecipient, ChildSupervisor)
  end),

  State = #state{
    socket    = Socket,
    listener  = Listener,
    child_sup = ChildSupervisor,
    command_recipient = CmdRecipient
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
    _Any ->
      io:fwrite("[indira TCP] call: ~p~n", [_Any]),
      {reply, ok, State}
  end.

handle_cast(_Request, State) ->
  io:fwrite("[indira TCP] cast: ~p~n", [_Request]),
  {noreply, State}.

handle_info(_Message, State) ->
  io:fwrite("[indira TCP] info: ~p~n", [_Message]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%-----------------------------------------------------------------------------
% networking internals
%-----------------------------------------------------------------------------

% resolve DNS address to IP
address_to_bind_option(any) ->
  [];
address_to_bind_option(Addr) when is_list(Addr) ->
  {ok, #hostent{h_addr_list = HostAddrs}} = inet:gethostbyname(Addr),
  [HostAddr | _Rest] = HostAddrs,
  [{ip, HostAddr}];
address_to_bind_option(Addr) when is_tuple(Addr) ->
  [{ip, Addr}].

%-----------------------------------------------------------------------------

% accept client connection and spawn new worker for it
acceptor_loop(Socket, CmdRecipient, ChildSupervisor) ->
  case gen_tcp:accept(Socket) of
    {ok, Client} ->
      io:fwrite("[indira TCP acceptor] spawning new client handler~n"),
      {ok, Worker} = indira_tcp_sup:new_worker(
        ChildSupervisor, [Client, CmdRecipient, self()]
      ),
      io:fwrite("[indira TCP acceptor] worker is at ~p~n", [Worker]),
      gen_tcp:controlling_process(Client, Worker),
      inet:setopts(Client, [{active, true}]),
      acceptor_loop(Socket, CmdRecipient, ChildSupervisor);
    {error, _Reason} ->
      % TODO: log this
      acceptor_loop(Socket, CmdRecipient, ChildSupervisor)
  end.

%-----------------------------------------------------------------------------

% read everything from TCP socket
% (not a part of gen_server; probably should get extracted)
reader_loop(Socket, CmdRecipient, Parent) ->
  receive
    {tcp_closed, Socket} ->
      gen_tcp:close(Socket),
      io:fwrite("[indira TCP client] stopping~n"),
      ok;
    {tcp, Socket, Line} ->
      io:fwrite("[indira TCP client] command ~p~n", [Line]),
      gen_server:call(CmdRecipient, {command, Line}),
      reader_loop(Socket, CmdRecipient, Parent);
    _Any ->
      % TODO: log this
      io:fwrite("[indira TCP client] unknown message: ~p~n", [_Any]),
      reader_loop(Socket, CmdRecipient, Parent)
  end.

%-----------------------------------------------------------------------------
% vim:ft=erlang:foldmethod=marker
