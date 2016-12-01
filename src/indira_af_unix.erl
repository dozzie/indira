%%%---------------------------------------------------------------------------
%%% @doc
%%%   AF_UNIX sockets.
%%%
%%%   The module mimics {@link gen_tcp} behaviour.
%%%
%%%   Active socket sends:
%%%   <ul>
%%%     <li>{@type @{unix, socket(), Data :: binary()@}}</li>
%%%     <li>{@type @{unix_closed, socket()@}}</li>
%%%     <li>{@type @{unix_error, socket(), Reason :: inet:posix()@}}</li>
%%%   </ul>
%%%
%%%   <b>Warning</b>: port driver that is a backend for this module uses
%%%   driver-level locking instead of port-level. Because of this, the module
%%%   <em>is not suitable</em> for heavy data load. For administrative
%%%   purposes it should be good enough, though.
%%%
%%% @todo Rewrite recv(), because it's fugly.
%%% @todo Add `{active, once}' option
%%% @todo Add `list' option.
%%% @todo Add `{packet, X}' option.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_af_unix).

%% server socket API
-export([listen/1, accept/1, accept/2]).
%% connection socket API
-export([connect/2, send/2, recv/2, recv/3]).
-export([controlling_process/2, setopts/2]).
%% common to server and connection sockets
-export([close/1]).
-export([format_error/1]).

-export_type([address/0, socket/0, server_socket/0, connection_socket/0]).

%%%---------------------------------------------------------------------------

-define(PORT_DRIVER_NAME, "af_unix_drv").
-define(APP_NAME, indira).

-define(LOOP_INTERVAL, 100). % 100ms

-define(PORT_COMMAND_ACCEPT,          133).
-define(PORT_COMMAND_RECV,            135).
-define(PORT_COMMAND_SET_ACTIVE,      136).
-define(PORT_COMMAND_SET_PASSIVE,     137).

%%%---------------------------------------------------------------------------

-type address() :: file:filename().
%% Socket address.

-type socket() :: port().
%% Socket handle.

-type connection_socket() :: socket().
%% Client socket handle (either from connecting to socket or from accepting
%% a connection on a server socket).

-type server_socket() :: socket().
%% Server (listening) socket handle.

-type option() :: {active, true | false}.
%% Socket configuration option.

%%%---------------------------------------------------------------------------
%%% server socket API
%%%---------------------------------------------------------------------------

%% @doc Setup a socket listening on specified address.

-spec listen(address()) ->
  {ok, server_socket()} | {error, inet:posix()}.

listen(Address) when is_binary(Address) ->
  listen(binary_to_list(Address));
listen(Address) when is_list(Address) ->
  spawn_driver(listen, Address).

%% @doc Accept a client connection.
%%   The function waits infinitely for a client.

-spec accept(server_socket()) ->
  {ok, connection_socket()} | {error, inet:posix()}.

accept(Socket) ->
  case try_accept(Socket) of
    {ok, Client} ->
      {ok, Client};
    nothing ->
      timer:sleep(?LOOP_INTERVAL),
      accept(Socket)
  end.

%% @doc Accept a client connection.

-spec accept(server_socket(), timeout()) ->
  {ok, connection_socket()} | {error, timeout | inet:posix()}.

accept(Socket, infinity = _Timeout) ->
  accept(Socket);

accept(Socket, Timeout) when Timeout =< 0 ->
  case try_accept(Socket) of
    {ok, Client} -> {ok, Client};
    nothing -> {error, timeout}
  end;

accept(Socket, Timeout) when Timeout =< ?LOOP_INTERVAL ->
  case try_accept(Socket) of
    {ok, Client} ->
      {ok, Client};
    nothing ->
      timer:sleep(Timeout),
      accept(Socket, 0)
  end;

accept(Socket, Timeout) when Timeout > ?LOOP_INTERVAL ->
  case try_accept(Socket) of
    {ok, Client} ->
      {ok, Client};
    nothing ->
      timer:sleep(?LOOP_INTERVAL),
      accept(Socket, Timeout - ?LOOP_INTERVAL)
  end.

%%%---------------------------------------------------------------------------
%%% connection socket
%%%---------------------------------------------------------------------------

%% @doc Connect to specified socket.
%%   Resulting connection defaults to passive (if not specified in options).
%%
%%   This function allows to send to datagram sockets as well, but
%%   {@link recv/2} and {@link recv/3} won't work on such socket.

-spec connect(address(), [option()]) ->
  {ok, connection_socket()} | {error, badarg | inet:posix()}.

connect(Address, Opts) when is_binary(Address) ->
  connect(binary_to_list(Address), Opts);
connect(Address, Opts) when is_list(Address) ->
  case spawn_driver(connect, Address) of
    {ok, Socket} ->
      case setopts(Socket, Opts) of
        ok -> {ok, Socket};
        {error, Reason} -> {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Send data to the socket.

-spec send(connection_socket(), iolist()) ->
  ok | {error, badarg}.

send(Socket, Data) ->
  try
    port_command(Socket, Data),
    ok
  catch
    error:badarg ->
      {error, badarg}
  end.

%% @doc Read `Length' bytes from socket.

-spec recv(connection_socket(), non_neg_integer()) ->
  {ok, binary()} | {error, closed | inet:posix()}.

recv(Socket, Length) ->
  case erlang:port_call(Socket, ?PORT_COMMAND_RECV, Length) of
    {ok, Packet} ->
      {ok, Packet};
    nothing ->
      timer:sleep(?LOOP_INTERVAL),
      recv(Socket, Length);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Read `Length' bytes from socket (with timeout).

-spec recv(connection_socket(), non_neg_integer(), timeout()) ->
  {ok, binary()} | {error, timeout | closed | inet:posix()}.

recv(Socket, Length, infinity = _Timeout) ->
  recv(Socket, Length);

recv(Socket, Length, Timeout) when Timeout =< 0 ->
  case erlang:port_call(Socket, ?PORT_COMMAND_RECV, Length) of
    {ok, Packet} -> {ok, Packet};
    nothing -> {error, timeout};
    {error, Reason} -> {error, Reason}
  end;

recv(Socket, Length, Timeout) when Timeout =< ?LOOP_INTERVAL ->
  case erlang:port_call(Socket, ?PORT_COMMAND_RECV, Length) of
    {ok, Packet} ->
      {ok, Packet};
    nothing ->
      timer:sleep(Timeout),
      recv(Socket, Length, 0);
    {error, Reason} ->
      {error, Reason}
  end;

recv(Socket, Length, Timeout) when Timeout > ?LOOP_INTERVAL ->
  case erlang:port_call(Socket, ?PORT_COMMAND_RECV, Length) of
    {ok, Packet} ->
      {ok, Packet};
    nothing ->
      timer:sleep(?LOOP_INTERVAL),
      recv(Socket, Length, Timeout - ?LOOP_INTERVAL);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Set the socket owner.

-spec controlling_process(socket(), pid()) ->
  ok | {error, not_owner | badarg}.

controlling_process(Socket, Pid) ->
  try
    Self = self(),
    case erlang:port_info(Socket, connected) of
      {connected, Pid} ->
        % new owner is the same as current, do nothing
        ok;
      {connected, Self} ->
        % new owner is different than current, and current is me
        true = port_connect(Socket, Pid),
        unlink(Socket),
        ok;
      {connected, _Any} ->
        {error, not_owner}
    end
  catch
    error:_ ->
      {error, badarg}
  end.

%% @doc Set socket options.

-spec setopts(socket(), [option()]) ->
  ok | {error, badarg}.

setopts(_Socket, [] = _Options) ->
  ok;

setopts(Socket, [{active, Active} | Rest] = _Options) ->
  case Active of
    true ->
      ok = erlang:port_call(Socket, ?PORT_COMMAND_SET_ACTIVE, ignore),
      setopts(Socket, Rest);
    false ->
      ok = erlang:port_call(Socket, ?PORT_COMMAND_SET_PASSIVE, ignore),
      setopts(Socket, Rest);
    _Any ->
      {error, badarg}
  end;

setopts(_Socket, [_Any | _Rest] = _Options) ->
  {error, badarg}.

%%%---------------------------------------------------------------------------
%%% common to server and connection
%%%---------------------------------------------------------------------------

%% @doc Close server connection.

-spec close(socket()) ->
  ok.

close(Socket) when is_port(Socket) ->
  try
    unlink(Socket),
    port_close(Socket),
    unload_driver()
  catch
    % this could be caused by port already being closed, which is expected for
    % `{active,true}' sockets
    error:badarg -> ignore
  end,
  ok.

%% @doc Convert a `Reason' from error tuple into usable error message.

-spec format_error(term()) ->
  string().

format_error(badarg    = _Reason) -> "invalid argument";
format_error(timeout   = _Reason) -> "operation timed out";
format_error(closed    = _Reason) -> "connection is closed";
format_error(not_owner = _Reason) -> "not the owner of the socket";
format_error(Reason) -> inet:format_error(Reason).

%%%---------------------------------------------------------------------------
%%% private helpers
%%%---------------------------------------------------------------------------

%% @doc Try accepting connection.

-spec spawn_driver(listen | connect, string()) ->
  {ok, socket()} | {error, inet:posix()}.

spawn_driver(Type, Address) ->
  DriverCommand = case Type of
    listen  -> ?PORT_DRIVER_NAME ++ " l:" ++ Address;
    connect -> ?PORT_DRIVER_NAME ++ " c:" ++ Address
  end,
  ok = load_driver(),
  try
    Port = open_port({spawn_driver, DriverCommand}, [binary]),
    {ok, Port}
  catch
    error:Reason ->
      _ = unload_driver(),
      {error, Reason}
  end.

%% @doc Try accepting connection.

-spec try_accept(server_socket()) ->
  {ok, connection_socket()} | nothing.

try_accept(Socket) ->
  case erlang:port_call(Socket, ?PORT_COMMAND_ACCEPT, ignore) of
    {ok, nothing} ->
      nothing;
    {ok, client} ->
      receive
        {Socket, {client, Client}} ->
          {ok, Client}
      end
  end.

%%----------------------------------------------------------

load_driver() ->
  erl_ddll:load(code:lib_dir(?APP_NAME, priv), ?PORT_DRIVER_NAME).

unload_driver() ->
  erl_ddll:unload(?PORT_DRIVER_NAME).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
