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
%%%   Errors returned by this module can all be parsed by {@link
%%%   indira_af_unix:format_error/1}.
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

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------
%%% types {{{

-define(UNIX_LISTEN_INTERVAL, 100).

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
          Result = indira_af_unix:recv(Sock, 0, Timeout),
          indira_af_unix:close(Sock),
          Result;
        {error, badarg} ->
          % closed socket; pretend it was "connection reset" error
          indira_af_unix:close(Sock),
          {error, econnreset}
      end;
    {error, Reason} ->
      {error, Reason}
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
      % TODO: sleep a little instead of making a tight infinite loop
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

%%%---------------------------------------------------------------------------
%%% public API for supervision tree
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start UDP listener process.

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
