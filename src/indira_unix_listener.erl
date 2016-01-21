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

-type connection() :: indira_af_unix:socket().

%%%---------------------------------------------------------------------------
%%% gen_indira_sock_stream callbacks
%%%---------------------------------------------------------------------------

%% @doc Prepare listening socket.

-spec listen(string()) ->
  {ok, #state{}} | {error, term()}.

listen(SocketPath) ->
  case indira_af_unix:listen(SocketPath) of
    {ok, Socket} ->
      {ok, #state{socket = Socket}};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Accept new connection.

-spec accept(#state{}) ->
    {ok, connection(), #state{}}
  | {ok, #state{}}
  | {stop, Reason :: term(), #state{}}.

accept(State = #state{socket = Socket}) ->
  % remember not to block forever here
  case indira_af_unix:accept(Socket, ?ACCEPT_TIMEOUT) of
    {ok, Connection} ->
      {ok, Connection, State};
    {error, timeout} ->
      {ok, State}
    % no more errors possible
    %{error, Reason} ->
    %  {stop, Reason, State}
  end.

%% @doc Set controlling process of newly accepted connection.

-spec controlling_process(connection(), pid()) ->
  ok | {error, term()}.

controlling_process(Connection = _ChildState, Pid) ->
  case indira_af_unix:controlling_process(Connection, Pid) of
    ok ->
      % once the socket has proper owner, make it active
      indira_af_unix:setopts(Connection, [{active, true}]);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Close listening socket.

-spec close(#state{}) ->
  ok.

close(_State = #state{socket = Socket}) ->
  indira_af_unix:close(Socket).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
