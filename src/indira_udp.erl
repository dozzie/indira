%%%---------------------------------------------------------------------------
%%% @doc
%%%   UDP listener entry point and worker process.
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
%%%   == Returned errors ==
%%%
%%%   Errors returned by this module can all (except for `{error,
%%%   timeout}') be parsed by {@link inet:format_error/1}.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_udp).

-behaviour(gen_indira_socket).
-behaviour(gen_server).

%% supervision tree API
-export([start_link/2]).

%% gen_indira_socket interface
-export([child_spec/1]).
-export([send_one_line/3, retry_send_one_line/3]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------
%%% types {{{

-record(state, {
  socket :: gen_udp:socket()
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
  case gen_udp:open(0, [{active, false}, binary]) of
    {ok, Sock} ->
      case gen_udp:send(Sock, Addr, Port, [Line, $\n]) of
        ok ->
          case gen_udp:recv(Sock, 0, Timeout) of
            % XXX: this will accept anything that was sent to this socket,
            % even if it didn't come from Addr:Port
            {ok, {_Addr, _Port, ReplyLine}} ->
              gen_udp:close(Sock),
              {ok, ReplyLine};
            {error, Reason} ->
              gen_udp:close(Sock),
              {error, Reason}
          end;
        {error, Reason} ->
          gen_udp:close(Sock),
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
%%   Hell, it's UDP. It doesn't refuse connections in any reliable way, so
%%   just do the same as {@link send_one_line/3}.

-spec retry_send_one_line(address(), iolist(), timeout()) ->
  {ok, iolist()} | {error, term()}.

retry_send_one_line(Address, Line, Timeout) ->
  send_one_line(Address, Line, Timeout).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% public API for supervision tree
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start UDP listener process.

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
      indira_log:set_context(udp, [{local, {BindAddr, BindPort}}]),
      State = #state{socket = Socket},
      {ok, State};
    {error, Reason} ->
      {stop, Reason}
  end.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Reason, _State = #state{socket = Socket}) ->
  gen_udp:close(Socket),
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

handle_info({udp, Socket, IP, Port, Line} = _Message,
            State = #state{socket = Socket}) ->
  inet:setopts(Socket, [{active, once}]),
  RoutingHint = {IP, Port},
  gen_indira_socket:command(Line, RoutingHint),
  {noreply, State};

handle_info({result, {IP, Port} = _RoutingHint, Line} = _Message,
            State = #state{socket = Socket}) ->
  gen_udp:send(Socket, IP, Port, [Line, "\n"]),
  {noreply, State};

handle_info({error, {IP, Port} = _RoutingHint, Type, Reason} = _Message,
            State) ->
  % TODO: log this event
  indira_log:warn("command execution error",
                  [{error_type, Type}, {error, Reason}, {peer, {IP, Port}}]),
  {noreply, State};

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
%%% network helpers
%%%---------------------------------------------------------------------------

%% @doc Prepare listening {@link gen_udp} socket.
%%
%% @todo IPv6 support

-spec listen(any | inet:hostname() | inet:ip_address(), inet:port_number()) ->
  {ok, gen_udp:socket()} | {error, {listen | resolve, term()}}.

listen(Address, Port) ->
  case bind_options(Address) of
    {ok, BindOptions} ->
      Options = [list, {active, once}, {reuseaddr, true} | BindOptions],
      case gen_udp:open(Port, Options) of
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
