%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   TCP listener process. TCP listener listens on TCP socket (surprise!),
%%%   accepts incoming connections and spawns for each of them a TCP reader
%%%   processes.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_tcp_listener).

-behaviour(gen_indira_sock_stream).

%% gen_indira_sock_stream callbacks
-export([listen/1, accept/1, controlling_process/2, close/1]).

%%%---------------------------------------------------------------------------

-define(ACCEPT_TIMEOUT, 300). % somewhat arbitrary

-record(state, {socket}).

%%%---------------------------------------------------------------------------

-type connection() :: gen_tcp:socket().

%%%---------------------------------------------------------------------------
%%% gen_indira_sock_stream callbacks
%%%---------------------------------------------------------------------------

%% @private
%% @doc Prepare listening socket.

-spec listen({inet:hostname() | inet:ip_address() | any,
              inet:port_number()}) ->
  {ok, #state{}} | {error, term()}.

listen({Host, Port} = _Args) ->
  ListenOpts = address_to_bind_option(Host) ++ [
    {active, false},
    {reuseaddr, true},
    list,
    {packet, line}
  ],
  case gen_tcp:listen(Port, ListenOpts) of
    {ok, Socket} ->
      {ok, #state{socket = Socket}};
    {error, Reason} ->
      {error, Reason}
  end.

%% @private
%% @doc Accept new connection.

-spec accept(#state{}) ->
    {ok, connection(), #state{}}
  | {ok, #state{}}
  | {stop, Reason :: term(), #state{}}.

accept(State = #state{socket = Socket}) ->
  % remember not to block forever here
  case gen_tcp:accept(Socket, ?ACCEPT_TIMEOUT) of
    {ok, Connection} ->
      {ok, Connection, State};
    {error, timeout} ->
      {ok, State};
    {error, Reason} ->
      {stop, Reason, State}
  end.

%% @private
%% @doc Set controlling process of newly accepted connection.

-spec controlling_process(connection(), pid()) ->
  ok | {error, term()}.

controlling_process(Connection = _ChildState, Pid) ->
  case gen_tcp:controlling_process(Connection, Pid) of
    ok ->
      inet:setopts(Connection, [{active, true}]);
    {error, Reason} ->
      {error, Reason}
  end.

%% @private
%% @doc Close listening socket.

-spec close(#state{}) ->
  ok.

close(_State = #state{socket = Socket}) ->
  gen_tcp:close(Socket).

%%%---------------------------------------------------------------------------
%%% network helpers
%%%---------------------------------------------------------------------------

%% @doc Resolve DNS address to IP.

-spec address_to_bind_option(any | inet:hostname() | inet:ip_address()) ->
  [{atom(), term()}].

address_to_bind_option(any) ->
  [];
address_to_bind_option(Addr) when is_list(Addr); is_atom(Addr) ->
  % TODO: IPv6 (inet6)
  {ok, HostAddr} = inet:getaddr(Addr, inet),
  [{ip, HostAddr}];
address_to_bind_option(Addr) when is_tuple(Addr) ->
  [{ip, Addr}].

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
