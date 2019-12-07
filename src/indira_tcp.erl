%%%---------------------------------------------------------------------------
%%% @doc
%%%   TCP listener entry point and process.
%%%
%%%   == Indira parameter ==
%%%
%%%   This module expects a tuple `{Host,Port}' as a parameter (see
%%%   {@link indira}). The `Host' part can be:
%%%   <ul>
%%%     <li>{@type inet:hostname()}</li>
%%%     <li>{@type inet:ip_address()} (i.e. `{N1,N2,N3,N4}' for IPv4)</li>
%%%     <li>`` 'any' '' to indicate no binding to any particular interface</li>
%%%   </ul>
%%%
%%%   === usage example ===
%%%
%```
%indira:indira_setup([
%  {listen, [
%    {indira_tcp, {"daemon.example.net", 1638}},
%    {indira_tcp, {{127,0,0,1}, 1639}},
%    {indira_tcp, {any, 1640}}
%  ]},
%  ...
%]).
%'''
%%%
%%%   == Returned errors ==
%%%
%%%   This module returns through
%%%   {@link gen_indira_socket:send_one_command/4} following errors:
%%%
%%%   <ul>
%%%     <li>`timeout' -- command send timeout timeout</li>
%%%     <li>`closed' -- socket closed during receiving reply</li>
%%%     <li>`badarg' -- invalid socket address</li>
%%%     <li>{@type inet:posix()}</li>
%%%   </ul>
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_tcp).

-behaviour(gen_indira_socket).
-behaviour(gen_server).

%% supervision tree API
-export([start_link/2]).

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

-define(TCP_LISTEN_INTERVAL, 100).
-define(MAX_LINE_LENGTH, 64 * 1024 * 1024).

-record(state, {
  socket :: gen_tcp:socket()
}).

-type address() :: {inet:hostname() | inet:ip_address(), inet:port_number()}.

%%% }}}
%%%---------------------------------------------------------------------------
%%% gen_indira_socket interface
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% child_spec() {{{

%% @private
%% @doc Socket listener child spec.

child_spec({Host, Port} = _BindAddr) ->
  {ignore,
    {?MODULE, start_link, [Host, Port]},
    permanent, 5000, worker, [?MODULE]}.

%% }}}
%%----------------------------------------------------------
%% send_one_line() {{{

%% @private
%% @doc Send a line to socket.

-spec send_one_line(address(), iolist(), timeout()) ->
  {ok, iolist()} | {error, term()}.

send_one_line({Addr, Port} = _Address, Line, Timeout) ->
  ConnectOpts = [
    {active, false},
    binary, {packet, line}, {packet_size, ?MAX_LINE_LENGTH}
  ],
  case gen_tcp:connect(Addr, Port, ConnectOpts) of
    {ok, Sock} ->
      case gen_tcp:send(Sock, [Line, $\n]) of
        ok ->
          Result = gen_tcp:recv(Sock, 0, Timeout),
          gen_tcp:close(Sock),
          Result;
        {error, Reason} ->
          gen_tcp:close(Sock),
          {error, Reason}
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

retry_send_one_line(Address, Line, infinity = _Timeout) ->
  case send_one_line(Address, Line, infinity) of
    {ok, Reply} -> {ok, Reply};
    % only keep trying when the host is up and reachable, but refuses
    % connections
    {error, econnrefused} -> retry_send_one_line(Address, Line, infinity);
    {error, Reason} -> {error, Reason}
  end;

retry_send_one_line(Address, Line, Timeout) when is_integer(Timeout) ->
  Timer = gen_indira_socket:setup_timer(Timeout),
  retry_send_one_line_loop(Address, Line, Timeout, Timer).

%% @doc Worker loop for {@link retry_send_one_line/3}.

-spec retry_send_one_line_loop(address(), iolist(), timeout(),
                               gen_indira_socket:timer()) ->
  {ok, iolist()} | {error, term()}.

retry_send_one_line_loop(Address, Line, Timeout, Timer) ->
  case send_one_line(Address, Line, Timeout) of
    {ok, Reply} ->
      gen_indira_socket:cancel_timer(Timer),
      {ok, Reply};
    % only keep trying when the host is up and reachable, but refuses
    % connections
    {error, econnrefused} ->
      case gen_indira_socket:timer_fired(Timer, 100) of
        true -> {error, timeout};
        false -> retry_send_one_line_loop(Address, Line, Timeout, Timer)
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
%% @doc Start TCP listener process.

start_link(Host, Port) ->
  gen_server:start_link(?MODULE, [Host, Port], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init([BindAddr, BindPort] = _Args) ->
  case listen(BindAddr, BindPort) of
    {ok, Socket} ->
      State = #state{socket = Socket},
      {ok, State, 0};
    {error, Reason} ->
      {stop, Reason}
  end.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Reason, _State = #state{socket = Socket}) ->
  gen_tcp:close(Socket),
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
  case gen_tcp:accept(Socket, ?TCP_LISTEN_INTERVAL) of
    {ok, Client} ->
      indira_tcp_conn:take_over(Client),
      {noreply, State, 0};
    {error, timeout} ->
      {noreply, State, 0};
    {error, Reason} ->
      {stop, {accept, Reason}, State}
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
%%% network helpers
%%%---------------------------------------------------------------------------

%% @doc Prepare listening {@link gen_tcp} socket.
%%
%% @todo IPv6 support

-spec listen(any | inet:hostname() | inet:ip_address(), inet:port_number()) ->
  {ok, gen_tcp:socket()} | {error, {listen | resolve, term()}}.

listen(Address, Port) ->
  case bind_options(Address) of
    {ok, BindOptions} ->
      Options = [
        list, {packet, line}, {active, false}, {packet_size, ?MAX_LINE_LENGTH},
        {reuseaddr, true}, {keepalive, true} |
        BindOptions
      ],
      case gen_tcp:listen(Port, Options) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} -> {error, {listen, Reason}}
      end;
    {error, Reason} ->
      {error, {resolve, Reason}}
  end.

%% @doc Resolve DNS address to IP.

-spec bind_options(any | inet:hostname() | inet:ip_address()) ->
  {ok, [{atom(), term()}]} | {error, term()}.

bind_options(any = _Addr) ->
  {ok, []};
bind_options(Addr) ->
  % TODO: IPv6 (inet6)
  case inet:getaddr(Addr, inet) of
    {ok, HostAddr} -> {ok, [{ip, HostAddr}]};
    {error, Reason} -> {error, Reason}
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
