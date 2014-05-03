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

-record(state, {router, socket}).

%%%---------------------------------------------------------------------------

%% @type connection() = {CommandRouter :: pid(), gen_tcp:socket()}.

-type connection() :: {pid(), gen_tcp:socket()}.

%%%---------------------------------------------------------------------------
%%% gen_indira_sock_stream callbacks
%%%---------------------------------------------------------------------------

%% @doc Prepare listening socket.
%%
%% @spec listen({CommandRouter :: pid(),
%%                {BindAddr :: string() | any, Port :: integer()}}) ->
%%   {ok, State :: #state{}} | {error, Reason}

-spec listen({pid(), {string() | any, integer()}}) ->
  {ok, #state{}} | {error, term()}.

listen({CommandRouter, {Host, Port}} = _Args) ->
  ListenOpts = address_to_bind_option(Host) ++ [
    {active, false},
    {reuseaddr, true},
    list,
    {packet, line}
  ],
  case gen_tcp:listen(Port, ListenOpts) of
    {ok, Socket} ->
      {ok, #state{router = CommandRouter, socket = Socket}};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Accept new connection.
%%
%% @spec accept(#state{}) ->
%%     {ok, connection(), #state{}}
%%   | {ok, #state{}}
%%   | {stop, Reason, #state{}}

-spec accept(#state{}) ->
    {ok, connection(), #state{}}
  | {ok, #state{}}
  | {stop, term(), #state{}}.

accept(State = #state{router = CommandRouter, socket = Socket}) ->
  % remember not to block forever here
  case gen_tcp:accept(Socket, ?ACCEPT_TIMEOUT) of
    {ok, Connection} ->
      {ok, {CommandRouter, Connection}, State};
    {error, timeout} ->
      {ok, State};
    {error, Reason} ->
      {stop, Reason, State}
  end.

%% @doc Set controlling process of newly accepted connection.
%%
%% @spec controlling_process(connection(), pid()) ->
%%   ok

-spec controlling_process(connection(), pid()) ->
  ok.

controlling_process({_CommandRouter, Connection} = _ChildState, Pid) ->
  case gen_tcp:controlling_process(Connection, Pid) of
    ok ->
      inet:setopts(Connection, [{active, true}]);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Close listening socket.

close(_State = #state{socket = Socket}) ->
  gen_tcp:close(Socket).

%%%---------------------------------------------------------------------------
%%% network helpers
%%%---------------------------------------------------------------------------

%% @doc Resolve DNS address to IP.

address_to_bind_option(any) ->
  [];
address_to_bind_option(Addr) when is_list(Addr) ->
  % TODO: IPv6 (inet6)
  {ok, HostAddr} = inet:getaddr(Addr, inet),
  [{ip, HostAddr}];
address_to_bind_option(Addr) when is_tuple(Addr) ->
  [{ip, Addr}].

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
