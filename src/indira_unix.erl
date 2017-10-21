%%%---------------------------------------------------------------------------
%%% @doc
%%%   AF_UNIX socket listener entry point and process.
%%%
%%%   == Indira parameter ==
%%%
%%%   This module expects a string `SocketPath' as a parameter (see
%%%   {@link indira}).
%%%
%%%   == Returned errors ==
%%%
%%%   This module returns through
%%%   {@link gen_indira_socket:send_one_command/4} following errors:
%%%
%%%   <ul>
%%%     <li>`timeout' -- command send timeout timeout</li>
%%%     <li>`closed' -- socket closed during receiving reply</li>
%%%     <li>`badarg' -- invalid socket path</li>
%%%     <li>{@type inet:posix()}</li>
%%%   </ul>
%%%
%%% @todo Support for mode and ownership.
%%% @todo Set `{active,once}' option, once it arrives to {@link
%%%   indira_af_unix}.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_unix).

-behaviour(gen_indira_socket).
-behaviour(gen_server).

%% supervision tree API
-export([start_link/1]).

%% gen_indira_socket interface
-export([child_spec/1]).
-export([send_one_line/3, retry_send_one_line/3]).
-export([format_error/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------
%%% types {{{

-define(UNIX_LISTEN_INTERVAL, 100).
-define(MAX_LINE_LENGTH, 1048576). % 1MB

-record(state, {
  socket :: indira_af_unix:server_socket()
}).

-type address() :: string().

%%% }}}
%%%---------------------------------------------------------------------------
%%% gen_indira_socket interface
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% child_spec() {{{

%% @private
%% @doc Socket listener child spec.

child_spec(SocketPath) ->
  {ignore,
    {?MODULE, start_link, [SocketPath]},
    permanent, 5000, worker, [?MODULE]}.

%% }}}
%%----------------------------------------------------------
%% send_one_line() {{{

%% @private
%% @doc Send a line to socket.

-spec send_one_line(address(), iolist(), timeout()) ->
  {ok, iolist()} | {error, term()}.

send_one_line(SocketPath, Line, Timeout) ->
  case indira_af_unix:connect(SocketPath, [{active, false}]) of
    {ok, Sock} ->
      case indira_af_unix:send(Sock, [Line, $\n]) of
        ok ->
          try
            ReplyLine = recv_line(Sock, ?MAX_LINE_LENGTH, Timeout),
            indira_af_unix:close(Sock),
            {ok, ReplyLine}
          catch
            error:Reason ->
              {error, Reason}
          end;
        {error, badarg} ->
          % closed socket; pretend it was "connection reset" error
          indira_af_unix:close(Sock),
          {error, econnreset}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Read a line from a socket.
%%
%%   Function raises an {@link erlang:error/1} on read errors.

-spec recv_line(indira_af_unix:connection_socket(), non_neg_integer(),
                timeout()) ->
  iolist() | no_return().

recv_line(_Socket, MaxLength, _Timeout) when MaxLength =< 0 ->
  erlang:error(emsgsize);
recv_line(Socket, MaxLength, Timeout) ->
  case indira_af_unix:recv(Socket, MaxLength, Timeout) of
    {ok, Chunk} ->
      case binary:last(Chunk) of
        $\n -> [Chunk];
        _ -> [Chunk | recv_line(Socket, MaxLength - size(Chunk), Timeout)]
      end;
    {error, Reason} ->
      erlang:error(Reason)
  end.

%% }}}
%%----------------------------------------------------------
%% retry_send_one_line() {{{

%% @private
%% @doc Send a line to socket, retrying when socket refuses connections.

-spec retry_send_one_line(address(), iolist(), timeout()) ->
  {ok, iolist()} | {error, term()}.

retry_send_one_line(SocketPath, Line, infinity = _Timeout) ->
  case send_one_line(SocketPath, Line, infinity) of
    {ok, Reply} ->
      {ok, Reply};
    {error, enoent} ->
      timer:sleep(100), % tight infinite loop unnecessarily consumes CPU time
      retry_send_one_line(SocketPath, Line, infinity);
    {error, Reason} ->
      {error, Reason}
  end;

retry_send_one_line(SocketPath, Line, Timeout) when is_integer(Timeout) ->
  Timer = gen_indira_socket:setup_timer(Timeout),
  retry_send_one_line_loop(SocketPath, Line, Timeout, Timer).

%% @doc Worker loop for {@link retry_send_one_line/3}.

-spec retry_send_one_line_loop(address(), iolist(), timeout(),
                               gen_indira_socket:timer()) ->
  {ok, iolist()} | {error, term()}.

retry_send_one_line_loop(SocketPath, Line, Timeout, Timer) ->
  case send_one_line(SocketPath, Line, Timeout) of
    {ok, Reply} ->
      gen_indira_socket:cancel_timer(Timer),
      {ok, Reply};
    {error, enoent} ->
      case gen_indira_socket:timer_fired(Timer, 100) of
        true -> {error, timeout};
        false -> retry_send_one_line_loop(SocketPath, Line, Timeout, Timer)
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% }}}
%%----------------------------------------------------------
%% format_error() {{{

%% @private
%% @doc Make a printable message from an error returned from a function from
%%   this module.

-spec format_error(Reason :: gen_indira_socket:error()) ->
  iolist().

format_error(badarg)  -> "invalid argument";
format_error(timeout) -> "operation timed out";
format_error(closed)  -> "connection is closed";
format_error(Reason) -> inet:format_error(Reason).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% public API for supervision tree
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start AF_UNIX listener process.

start_link(SocketPath) ->
  gen_server:start_link(?MODULE, [SocketPath], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init([SocketPath] = _Args) ->
  case indira_af_unix:listen(SocketPath) of
    {ok, Socket} ->
      State = #state{socket = Socket},
      {ok, State, 0};
    {error, Reason} ->
      {stop, Reason}
  end.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Reason, _State = #state{socket = Socket}) ->
  indira_af_unix:close(Socket),
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

%% unknown calls
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State, 0}.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

%% unknown casts
handle_cast(_Request, State) ->
  {noreply, State, 0}.

%% @private
%% @doc Handle incoming messages.

handle_info(timeout = _Message, State = #state{socket = Socket}) ->
  case indira_af_unix:accept(Socket, ?UNIX_LISTEN_INTERVAL) of
    {ok, Client} ->
      indira_unix_conn:take_over(Client),
      {noreply, State, 0};
    {error, timeout} ->
      {noreply, State, 0}
    %{error, Reason} -> % never matches
    %  {stop, {accept, Reason}, State}
  end;

%% unknown messages
handle_info(_Message, State) ->
  {noreply, State, 0}.

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
