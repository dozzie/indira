%-----------------------------------------------------------------------------
% TCP listener
%-----------------------------------------------------------------------------

-module(indira_tcp).

-behaviour(indira_listener).

% Indira listener API
-export([supervision_child_spec/2]).

% public API for supervision tree
-export([start_link/4]).
-export([start_link_worker/2]).

%-----------------------------------------------------------------------------

-include_lib("kernel/include/inet.hrl").

-record(state, {socket, worker_pool_sup, command_recipient}).

%-----------------------------------------------------------------------------
% Indira listener API
%-----------------------------------------------------------------------------

supervision_child_spec(CmdRecipient, {Host, Port} = _Args) ->
  MFA = {indira_tcp_sup, start_link, [CmdRecipient, Host, Port]},
  {MFA, supervisor}.

% could also be:
%   MFA = {?MODULE, start_link, [args, to, self]},
%   {MFA, worker}.

%-----------------------------------------------------------------------------
% public API for supervision tree
%-----------------------------------------------------------------------------

% spawn process that listens on TCP socket, accepts connections and spawns
% reader workers
start_link(Supervisor, CmdRecipient, Host, Port) ->
  {_, Ref} = Parent = {self(), make_ref()},
  Pid = spawn_link(fun() ->
    % NOTE: most probably `Parent = Supervisor', but this way I don't insist
    % on it
    start_acceptor(Parent, Supervisor, CmdRecipient, Host, Port)
  end),
  % synchronize with child (will be killed by link on error)
  receive
    {Ref, bound} -> ok
  end,
  {ok, Pid}.

% spawn process that reads everything from TCP socket
start_link_worker(CmdRecipient, ClientSocket) ->
  Pid = spawn_link(fun() ->
    start_worker(ClientSocket, CmdRecipient)
  end),
  {ok, Pid}.

%-----------------------------------------------------------------------------
% connection acceptor
%-----------------------------------------------------------------------------

start_acceptor({Parent, Ref}, Supervisor, CmdRecipient, Address, Port) ->
  % create listening socket
  BindOpt = address_to_bind_option(Address),
  {ok, Socket} = gen_tcp:listen(Port, BindOpt ++ [
    {active, false}, {reuseaddr, true}, list, {packet, line}
  ]),
  % synchronize with parent
  Parent ! {Ref, bound},

  % NOTE: this call must go after synchronizing to parent, since the
  % `Supervisor' still waits for `Parent' to return
  {ok, WorkerPoolSup} =
    indira_tcp_sup:start_worker_pool(Supervisor, CmdRecipient),

  State = #state{
    socket = Socket,
    worker_pool_sup = WorkerPoolSup,
    command_recipient = CmdRecipient
  },
  acceptor_loop(State).

% accept client connection and spawn new worker for it
acceptor_loop(State) ->
  % TODO: code change?
  % TODO: consume messages so mailbox won't fill up
  case gen_tcp:accept(State#state.socket) of
    {ok, Client} ->
      % spawn new worker
      Supervisor = State#state.worker_pool_sup,
      {ok, Worker} = indira_tcp_sup:new_worker(Supervisor, Client),

      % assign client socket to the newly spawned worker and make it active
      gen_tcp:controlling_process(Client, Worker),
      inet:setopts(Client, [{active, true}]),

      acceptor_loop(State);

    {error, _Reason} ->
      % TODO: log this
      acceptor_loop(State)
  end.

%-----------------------------------------------------------------------------
% per-connection (per-client) worker
%-----------------------------------------------------------------------------

% This function has two reasons to exist:
%   * it's similar to acceptor's structure
%   * I may add informing CmdRecipient about new connection
start_worker(ClientSocket, CmdRecipient) ->
  worker_loop(ClientSocket, CmdRecipient).

% read everything from TCP socket
worker_loop(ClientSocket, CmdRecipient) ->
  % TODO: code change?
  receive
    {tcp_closed, ClientSocket} ->
      gen_tcp:close(ClientSocket),
      ok;
    {tcp, ClientSocket, Line} ->
      indira:command(CmdRecipient, Line),
      worker_loop(ClientSocket, CmdRecipient);
    _Any ->
      % TODO: log this
      io:fwrite("[indira TCP client] got a message: ~p~n", [_Any]),
      worker_loop(ClientSocket, CmdRecipient)
  end.

%-----------------------------------------------------------------------------
% network helpers
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
% vim:ft=erlang:foldmethod=marker
