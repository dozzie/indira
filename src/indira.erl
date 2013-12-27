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

-record(state, {
  sockets = []
}).

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
  ListenSockets = [listen(Proto, A, P) || {Proto, A, P} <- ListenSpec],
  State = #state{sockets = ListenSockets},
  {ok, State}.

terminate(_Reason, State) ->
  io:fwrite("[indira stopping] unlinking children~n"),
  [unlink(Pid) || {tcp, _Sock, Pid} <- State#state.sockets],
  io:fwrite("[indira stopping] killing children~n"),
  [exit(Pid, stahp) || {tcp, _Sock, Pid} <- State#state.sockets],
  ok.

handle_call(Request, _From, State) ->
  case Request of
    stop ->
      io:fwrite("[indira stopping]~n"),
      {stop, normal, ok, State};
    %{'EXIT', _Pid, shutdown} ->
    %  io:fwrite("[indira stopping on shutdown]~n"),
    %  {stop, shutdown, ok, State};
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
% networking internals
%-----------------------------------------------------------------------------

listen(Proto, Addr, Port) when Proto == tcp andalso is_list(Addr) ->
  listen(Proto, resolve(Addr), Port);

listen(tcp, Addr, Port) ->
  BindOpt = case Addr of
    any -> [];
    _   -> [{ip, Addr}]
  end,
  {ok, Sock} = gen_tcp:listen(Port, BindOpt ++ [
    {active, false}, {reuseaddr, true},
    list, {packet, line}
  ]),
  {ok, Pid} = start_link_acceptor({tcp, Sock}),
  ok = gen_tcp:controlling_process(Sock, Pid),
  {tcp, Sock, Pid}.

%-----------------------------------------------------------------------------

start_link_acceptor({tcp, Sock} = _SockSpec) ->
  Self = self(),
  Pid = spawn_link(fun() ->
    acceptor_loop(Sock, Self)
  end),
  io:fwrite("<indira> new acceptor: ~p~n", [Pid]),
  {ok, Pid}.

acceptor_loop(Sock, Parent) ->
  case gen_tcp:accept(Sock) of
    {ok, Client} ->
      Acceptor = self(),
      spawn_link(fun() ->
        % TODO: send commands to parent
        Message = io_lib:format("unimplemented~n"
                                "acceptor: ~p~n"
                                "parent: ~p~n",
                                [Acceptor, Parent]),
        gen_tcp:send(Client, Message),
        {ok, Peer} = inet:peername(Client),
        Parent ! {client, Peer},
        gen_tcp:close(Client),
        ok
      end),
      acceptor_loop(Sock, Parent);
    {error, _Reason} ->
      % TODO: log this
      acceptor_loop(Sock, Parent)
  end.

%-----------------------------------------------------------------------------

resolve(Addr) ->
  {ok, #hostent{h_addr_list = HostAddrs}} = inet:gethostbyname(Addr),
  hd(HostAddrs).

%-----------------------------------------------------------------------------
% vim:ft=erlang:foldmethod=marker
