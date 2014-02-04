%%%---------------------------------------------------------------------------
%%%
%%% TCP listener.
%%%
%%%---------------------------------------------------------------------------

-module(indira_tcp_listener).

-behaviour(gen_server).

%% public API for supervision tree
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-define(ACCEPT_TIMEOUT, 300). % somewhat arbitrary

-define(NORETURN       (State),            {noreply,             State, 0}).
-define(STOP_RETURN(Reason, Reply, State), {stop, Reason, Reply, State}).
-define(INIT_OK    (State),  {ok, State, 0}).
-define(CODE_CHANGE(State),  {ok, State}). % NOTE: timeout not applicable

%%%---------------------------------------------------------------------------

-include_lib("kernel/include/inet.hrl").

-record(listen, {socket, worker_pool_sup}).

%%%---------------------------------------------------------------------------
%%% public API for supervision tree
%%%---------------------------------------------------------------------------

%% spawn process that listens on TCP socket, accepts connections and spawns
%% reader workers
start_link(Supervisor, Host, Port) ->
  gen_server:start_link(?MODULE, {Supervisor, Host, Port}, []).

%%%---------------------------------------------------------------------------
%%% connection acceptor
%%%---------------------------------------------------------------------------

init({Supervisor, Host, Port} = _Args) ->
  % create listening socket
  BindOpt = address_to_bind_option(Host),
  {ok, Socket} = gen_tcp:listen(Port, BindOpt ++ [
    {active, false}, {reuseaddr, true}, list, {packet, line}
  ]),

  % first thing to do after this call is finished, the workers pool must be
  % retrieved
  % TODO: explain why there's no race condition between gen_tcp:accept() and
  % ?MODULE:handle_info()
  self() ! {start_worker_pool, Supervisor},

  State = #listen{socket = Socket, worker_pool_sup = undefined},
  ?INIT_OK(State).


terminate(_Reason, _State = #listen{socket = Socket}) ->
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


handle_info({start_worker_pool, Supervisor}, State = #listen{}) ->
  % retrieve workers pool, as promised in init(listener)
  {ok, WorkerPoolSup} =
    indira_tcp_sup:start_worker_pool(Supervisor),
  NewState = State#listen{worker_pool_sup = WorkerPoolSup},
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

%% ignore other messages
handle_info(_Any, State = #listen{}) ->
  ?NORETURN(State).

%%%---------------------------------------------------------------------------
%%% network helpers
%%%---------------------------------------------------------------------------

%% resolve DNS address to IP
address_to_bind_option(any) ->
  [];
address_to_bind_option(Addr) when is_list(Addr) ->
  {ok, #hostent{h_addr_list = HostAddrs}} = inet:gethostbyname(Addr),
  [HostAddr | _Rest] = HostAddrs,
  [{ip, HostAddr}];
address_to_bind_option(Addr) when is_tuple(Addr) ->
  [{ip, Addr}].

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
