%%%---------------------------------------------------------------------------
%%% @doc
%%%   UNIX socket listener entry point.
%%%
%%%   == Indira parameter ==
%%%
%%%   This module expects a string `SocketPath' as a parameter (see
%%%   {@link indira}).
%%%
%%%   == Returned errors ==
%%%
%%%   Errors returned by this module can all (except for `{error,
%%%   timeout}') be parsed by {@link inet:format_error/1}.
%%%
%%% @TODO Support for mode and ownership.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_unix).

-behaviour(gen_indira_listener).

%% Indira listener API
-export([child_spec/1]).
-export([send_one_line/3, retry_send_one_line/3]).

%%%---------------------------------------------------------------------------
%%% Indira listener API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Listener description.

child_spec(SocketPath) ->
  SockStreamSupArgs = [
    indira_unix_listener,
    {indira_unix_reader, start_link},
    SocketPath
  ],
  {ignore,
    {indira_sock_stream_sup, start_link, SockStreamSupArgs},
    permanent, 5000, supervisor, [indira_sock_stream_sup]}.

%% @private
%% @doc Send a line to socket.

send_one_line(Address, Line, Timeout) ->
  case indira_af_unix:connect(Address, [{active, false}]) of
    {ok, Sock} ->
      case indira_af_unix:send(Sock, [Line, $\n]) of
        ok ->
          case indira_af_unix:recv(Sock, 0, Timeout) of
            {ok, Result} ->
              indira_af_unix:close(Sock),
              {ok, Result};
            {error, closed} ->
              % unexpected EOF; close enough to "connection reset" error to
              % pretend it was one
              indira_af_unix:close(Sock),
              {error, econnreset};
            {error, Reason} ->
              indira_af_unix:close(Sock),
              {error, Reason}
          end;
        {error, _Reason} ->
          % only `{error,badarg}' can get here; pretend it was "connection
          % reset" error
          indira_af_unix:close(Sock),
          {error, econnreset}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @private
%% @doc Send a line to socket, retrying when socket doesn't exist.

retry_send_one_line(Address, Line, infinity = _Timeout) ->
  case send_one_line(Address, Line, infinity) of
    {ok, Reply} -> {ok, Reply};
    {error, enoent} -> retry_send_one_line(Address, Line, infinity);
    {error, Reason} -> {error, Reason}
  end;

retry_send_one_line(Address, Line, Timeout) when is_integer(Timeout) ->
  Timer = gen_indira_listener:setup_timer(Timeout),
  retry_send_one_line_loop(Address, Line, Timeout, Timer).

%% @doc Worker loop for {@link retry_send_one_line/3}.

-spec retry_send_one_line_loop(string(), iolist(), timeout(),
                               gen_indira_listener:timer()) ->
  {ok, iolist()} | {error, term()}.

retry_send_one_line_loop(Address, Line, Timeout, Timer) ->
  case send_one_line(Address, Line, Timeout) of
    {ok, Reply} ->
      gen_indira_listener:cancel_timer(Timer),
      {ok, Reply};
    {error, enoent} ->
      case gen_indira_listener:timer_fired(Timer, 100) of
        true -> {error, timeout};
        false -> retry_send_one_line_loop(Address, Line, Timeout, Timer)
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
