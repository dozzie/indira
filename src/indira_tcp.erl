%%%---------------------------------------------------------------------------
%%% @doc
%%%   TCP listener entry point.
%%%   This listener is implemented as {@link gen_indira_sock_stream} module
%%%   (listener and connection handler separated). It should be simple enough
%%%   to serve as an example for your own code, if needed.
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

-module(indira_tcp).

-behaviour(gen_indira_listener).

%% Indira listener API
-export([child_spec/1]).
-export([send_one_line/3, retry_send_one_line/3]).

%%%---------------------------------------------------------------------------
%%% Indira listener API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Listener description.

child_spec({_Host,_Port} = BindAddr) ->
  SockStreamSupArgs = [
    indira_tcp_listener,
    {indira_tcp_reader, start_link},
    BindAddr
  ],
  {ignore,
    {indira_sock_stream_sup, start_link, SockStreamSupArgs},
    permanent, 5000, supervisor, [indira_sock_stream_sup]}.

%% @private
%% @doc Send a line to socket.

-spec send_one_line({inet:hostname() | inet:ip_address(), inet:port_number()},
                    iolist(), timeout()) ->
  {ok, iolist()} | {error, term()}.

send_one_line({Addr, Port} = _Address, Line, Timeout) ->
  case gen_tcp:connect(Addr, Port, [{active, false}, binary, {packet, line}]) of
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

%% @private
%% @doc Send a line to socket, retrying when socket refuses connections.

-spec retry_send_one_line({inet:hostname() | inet:ip_address(),
                            inet:port_number()},
                          iolist(), timeout()) ->
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
  Timer = gen_indira_listener:setup_timer(Timeout),
  retry_send_one_line_loop(Address, Line, Timeout, Timer).

%% @doc Worker loop for {@link retry_send_one_line/3}.

-spec retry_send_one_line_loop({inet:hostname() | inet:ip_address(),
                                 inet:port_number()},
                               iolist(), timeout(),
                               gen_indira_listener:timer()) ->
  {ok, iolist()} | {error, term()}.

retry_send_one_line_loop(Address, Line, Timeout, Timer) ->
  case send_one_line(Address, Line, Timeout) of
    {ok, Reply} ->
      gen_indira_listener:cancel_timer(Timer),
      {ok, Reply};
    % only keep trying when the host is up and reachable, but refuses
    % connections
    {error, econnrefused} ->
      case gen_indira_listener:timer_fired(Timer, 100) of
        true -> {error, timeout};
        false -> retry_send_one_line_loop(Address, Line, Timeout, Timer)
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
