%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   UNIX listener process.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_unix_listener).

-behaviour(gen_indira_sock_stream).

%% gen_indira_sock_stream callbacks
-export([listen/1, accept/1, controlling_process/2, close/1]).

%%%---------------------------------------------------------------------------

-define(ACCEPT_TIMEOUT, 300). % somewhat arbitrary

-record(state, {router, socket}).

%%%---------------------------------------------------------------------------

%% @type command_router() = pid() | atom().

-type command_router() :: pid() | atom().

%% @type connection() = {command_router(), indira_af_unix:connection_socket()}.

-type connection() :: {command_router(), indira_af_unix:connection_socket()}.

%%%---------------------------------------------------------------------------
%%% gen_indira_sock_stream callbacks
%%%---------------------------------------------------------------------------

%% @doc Prepare listening socket.
%%
%% @spec listen({command_router(), SocketPath :: string()}) ->
%%   {ok, State :: #state{}} | {error, Reason}

-spec listen({command_router(), string()}) ->
  {ok, #state{}} | {error, term()}.

listen({CommandRouter, SocketPath} = _Args) ->
  case indira_af_unix:listen(SocketPath) of
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
  case indira_af_unix:accept(Socket, ?ACCEPT_TIMEOUT) of
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
  ok | {error, term()}.

controlling_process({_CommandRouter, Connection} = _ChildState, Pid) ->
  case indira_af_unix:controlling_process(Connection, Pid) of
    ok ->
      % once the socket has proper owner, make it active
      indira_af_unix:setopts(Connection, [{active, true}]);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Close listening socket.

close(_State = #state{socket = Socket}) ->
  indira_af_unix:close(Socket).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
