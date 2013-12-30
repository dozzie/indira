%-----------------------------------------------------------------------------
% TCP listener
%-----------------------------------------------------------------------------

-module(indira_tcp).

-behaviour(indira_listener).
-behaviour(gen_server).

%-----------------------------------------------------------------------------

% Indira listener API
-export([supervision_child_spec/2]).

% gen_server API
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

% public API for supervision tree
-export([start_link/4]).
-export([start_link_worker/2]).

%-----------------------------------------------------------------------------

-define(ACCEPT_TIMEOUT, 300). % somewhat arbitrary

-define(RETURN  (Reply, State),            {  reply,      Reply, State, 0}).
-define(NORETURN       (State),            {noreply,             State, 0}).
-define(STOP_RETURN(Reason, Reply, State), {stop, Reason, Reply, State}).
-define(STOP       (Reason,        State), {stop, Reason,        State}).
-define(INIT_OK    (State),  {ok, State, 0}).
-define(CODE_CHANGE(State),  {ok, State}). % NOTE: timeout not applicable

%-----------------------------------------------------------------------------

-include_lib("kernel/include/inet.hrl").

-record(listen, {socket, worker_pool_sup}).
-record(client, {socket, command_recipient}).

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
  Args = [listener, Supervisor, CmdRecipient, Host, Port],
  gen_server:start_link(?MODULE, Args, []).

% spawn process that reads everything from TCP socket
start_link_worker(CmdRecipient, ClientSocket) ->
  Args = [worker, CmdRecipient, ClientSocket],
  gen_server:start_link(?MODULE, Args, []).

%-----------------------------------------------------------------------------
% connection acceptor
%-----------------------------------------------------------------------------

init([listener, Supervisor, CmdRecipient, Host, Port]) ->
  % create listening socket
  BindOpt = address_to_bind_option(Host),
  {ok, Socket} = gen_tcp:listen(Port, BindOpt ++ [
    {active, false}, {reuseaddr, true}, list, {packet, line}
  ]),

  % first thing to do after this call is finished, the workers pool must be
  % retrieved
  % TODO: explain why there's no race condition between gen_tcp:accept() and
  % ?MODULE:handle_info()
  self() ! {start_worker_pool, Supervisor, CmdRecipient},

  State = #listen{socket = Socket, worker_pool_sup = undefined},
  ?INIT_OK(State);

init([worker, CmdRecipient, ClientSocket]) ->
  State = #client{socket = ClientSocket, command_recipient = CmdRecipient},
  ?INIT_OK(State).


% @private
% retrieve workers pool, as promised in init(listener)
start_worker_pool(Supervisor, CmdRecipient,
                  State = #listen{worker_pool_sup = undefined}) ->
  {ok, WorkerPoolSup} =
    indira_tcp_sup:start_worker_pool(Supervisor, CmdRecipient),
  State#listen{worker_pool_sup = WorkerPoolSup}.


terminate(_Reason, _State = #listen{socket = Socket}) ->
  gen_tcp:close(Socket),
  ok;

terminate(_Reason, _State = #client{socket = Socket}) ->
  gen_tcp:close(Socket),
  ok.


code_change(_OldVsn, State, _Extra) ->
  ?CODE_CHANGE(State).


handle_call(stop, _From, State) ->
  ?STOP_RETURN(normal, ok, State);
handle_call(_Request, _From, State) ->
  ?NORETURN(State). % ignore unknown calls


handle_cast(_Request, State) ->
  ?NORETURN(State). % ignore unknown casts


handle_info({start_worker_pool, Sup, Cmd}, State = #listen{}) ->
  NewState = start_worker_pool(Sup, Cmd, State),
  ?NORETURN(NewState);

handle_info(timeout, State = #listen{}) ->
  #listen{socket = Socket, worker_pool_sup = Supervisor} = State,

  case gen_tcp:accept(Socket, ?ACCEPT_TIMEOUT) of
    {ok, Client} ->
      % spawn new worker
      {ok, Worker} = indira_tcp_sup:new_worker(Supervisor, Client),

      % assign client socket to the newly spawned worker and make it active
      gen_tcp:controlling_process(Client, Worker),
      inet:setopts(Client, [{active, true}]),

      ?NORETURN(State);

    {error, timeout} ->
      % OK, expected thing
      ?NORETURN(State);

    {error, _Reason} ->
      % TODO: log this
      ?NORETURN(State)
  end;

% ignore other messages
handle_info(_Any, State = #listen{}) ->
  ?NORETURN(State);

handle_info({tcp_closed, Socket}, State = #client{socket = Socket}) ->
  gen_tcp:close(Socket),
  ?STOP(normal, State);

handle_info({tcp, Socket, Line}, State = #client{socket = Socket}) ->
  indira:command(State#client.command_recipient, Line),
  ?NORETURN(State);

handle_info(_Any, State = #client{}) ->
  ?NORETURN(State).

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
