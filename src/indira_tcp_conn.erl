%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   TCP connection handler for {@link indira_tcp}.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_tcp_conn).

-behaviour(gen_server).

%% public interface
-export([take_over/1]).

%% supervision tree API
-export([start/1, start_link/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------
%%% types {{{

-record(state, {
  socket :: gen_tcp:socket()
}).

%%% }}}
%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Spawn a worker process, taking over a client socket.
%%
%%   The caller must be the controlling process of the `Socket'.
%%
%%   In case of spawning error, the socket is closed. In any case, caller
%%   shouldn't bother with the socket anymore.

-spec take_over(gen_tcp:socket()) ->
  {ok, pid()} | {error, term()}.

take_over(Socket) ->
  case indira_tcp_conn_sup:spawn_worker(Socket) of
    {ok, Pid} ->
      ok = gen_tcp:controlling_process(Socket, Pid),
      inet:setopts(Socket, [{active, once}]),
      {ok, Pid};
    {error, Reason} ->
      gen_tcp:close(Socket),
      {error, Reason}
  end.

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start TCP reader process.

start(Socket) ->
  gen_server:start(?MODULE, [Socket], []).

%% @private
%% @doc Start TCP reader process.

start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init([Socket] = _Args) ->
  {ok, LocalAddr} = inet:sockname(Socket),
  {ok, PeerAddr} = inet:peername(Socket),
  indira_log:set_context(tcp, [
    {local, LocalAddr},
    {peer, PeerAddr}
  ]),
  State = #state{socket = Socket},
  {ok, State}.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Arg, _State = #state{socket = Socket}) ->
  gen_tcp:close(Socket),
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

%% unknown calls
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

%% unknown casts
handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
%% @doc Handle incoming messages.

handle_info({result, Line} = _Message,
            State = #state{socket = Socket}) ->
  case gen_tcp:send(Socket, [Line, $\n]) of
    ok ->
      % read another request
      inet:setopts(Socket, [{active, once}]),
      {noreply, State};
    {error, Reason} ->
      indira_log:info("connection error", [{error, Reason}]),
      {stop, normal, State}
  end;

handle_info({error, Type, Reason} = _Message, State) ->
  indira_log:warn("command execution error",
                  [{error_type, Type}, {error, Reason}]),
  {stop, normal, State};

handle_info({error, Type, Reason, StackTrace} = _Message, State) ->
  indira_log:warn("command execution error",
                  [{error_type, Type}, {error, Reason}, {stack, StackTrace}]),
  {stop, normal, State};

handle_info({tcp, Socket, Line} = _Message,
            State = #state{socket = Socket}) ->
  % XXX: don't set `{active, once}'; wait for reply to arrive
  gen_indira_socket:command(Line),
  {noreply, State};

handle_info({tcp_closed, Socket} = _Message,
            State = #state{socket = Socket}) ->
  {stop, normal, State};

handle_info({tcp_error, Socket, Reason} = _Message,
            State = #state{socket = Socket}) ->
  indira_log:info("connection error", [{error, Reason}]),
  {stop, normal, State};

%% unknown messages
handle_info(_Message, State) ->
  {noreply, State}.

%% }}}
%%----------------------------------------------------------
%% code change {{{

%% @private
%% @doc Handle code change.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
