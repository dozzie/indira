%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   AF_UNIX connection handler for {@link indira_unix}.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_unix_conn).

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
  socket :: indira_af_unix:connection_socket()
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

-spec take_over(indira_af_unix:connection_socket()) ->
  {ok, pid()} | {error, term()}.

take_over(Socket) ->
  case indira_unix_conn_sup:spawn_worker(Socket) of
    {ok, Pid} ->
      ok = indira_af_unix:controlling_process(Socket, Pid),
      % FIXME: `indira_af_unix' doesn't support `{active, once}' sockets yet
      indira_af_unix:setopts(Socket, [{active, true}]),
      {ok, Pid};
    {error, Reason} ->
      indira_af_unix:close(Socket),
      {error, Reason}
  end.

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start AF_UNIX reader process.

start(Socket) ->
  gen_server:start(?MODULE, [Socket], []).

%% @private
%% @doc Start AF_UNIX reader process.

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
  % TODO: socket address (path)
  indira_log:set_context(unix, []),
  State = #state{socket = Socket},
  {ok, State}.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Arg, _State = #state{socket = Socket}) ->
  indira_af_unix:close(Socket),
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
  case indira_af_unix:send(Socket, [Line, $\n]) of
    ok ->
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

handle_info({unix, Socket, Line} = _Message,
            State = #state{socket = Socket}) ->
  gen_indira_socket:command(Line),
  {noreply, State};

handle_info({unix_closed, Socket} = _Message,
            State = #state{socket = Socket}) ->
  {stop, normal, State};

handle_info({unix_error, Socket, Reason} = _Message,
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
