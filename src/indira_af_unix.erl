%%%---------------------------------------------------------------------------
%%% @doc
%%%   Module for working with AF_UNIX connections.
%%%   The module mimics {@link gen_tcp} behaviour.
%%%
%%%   Active socket sends:
%%%   <ul>
%%%     <li>`{unix, Socket, Data :: binary()}'</li>
%%%     <li>`{unix_closed, Socket}'</li>
%%%     <li>`{unix_error, Socket, Reason}'</li>
%%%   </ul>
%%%
%%%   @TODO Rewrite recv(), because it's fugly.
%%%   @TODO Add `{active, once}' option
%%%   @TODO Add `list' option.
%%%   @TODO Add `{packet, X}' option.
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

-export_type([socket/0]).

%%%---------------------------------------------------------------------------

-define(PORT_DRIVER_NAME, "af_unix_drv").
-define(APP_NAME, indira).

-define(LOOP_INTERVAL, 100). % 100ms

-define(PORT_COMMAND_ACCEPT,          133).
-define(PORT_COMMAND_RECV,            135).
-define(PORT_COMMAND_SET_ACTIVE,      136).
-define(PORT_COMMAND_SET_PASSIVE,     137).

%%%---------------------------------------------------------------------------

%% @type socket() = port().

-type socket() :: port().

%% @type connection_socket() = socket().

-type connection_socket() :: socket().

%% @type server_socket() = socket().

-type server_socket() :: socket().

%% @type option() = {active, true | false}.

-type option() :: {active, true | false}.

%%%---------------------------------------------------------------------------
%%% server socket API
%%%---------------------------------------------------------------------------

%% @doc Setup a socket listening on specified address.
%%
%% @spec listen(string()) ->
%%   {ok, server_socket()} | {error, Reason}

-spec listen(string()) ->
  {ok, server_socket()} | {error, term()}.

listen(Address) ->
  spawn_driver(listen, Address).

%% @doc Accept a client connection.
%%   The function waits infinitely for a client.
%%
%% @spec accept(server_socket()) ->
%%   {ok, connection_socket()} | {error, Reason}

-spec accept(server_socket()) ->
  {ok, connection_socket()} | {error, term()}.

accept(Socket) ->
  case try_accept(Socket) of
    {ok, Client} ->
      {ok, Client};
    nothing ->
      timer:sleep(?LOOP_INTERVAL),
      accept(Socket)
  end.

%% @doc Accept a client connection.
%%
%% @spec accept(server_socket(), non_neg_integer()) ->
%%   {ok, connection_socket()} | {error, timeout} | {error, Reason}

-spec accept(server_socket(), non_neg_integer()) ->
  {ok, connection_socket()} | {error, timeout} | {error, term()}.

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
%%
%% @spec connect(string(), [option()]) ->
%%   {ok, connection_socket()} | {error, Reason}

connect(Address, Opts) ->
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
%%
%% @spec send(connection_socket(), iolist() | binary()) ->
%%   ok | {error, Reason}

send(Socket, Data) ->
  try
    port_command(Socket, Data),
    ok
  catch
    error:badarg ->
      {error, badarg}
  end.

%% @doc Read `Length' bytes from socket.
%%
%% @spec recv(connection_socket(), non_neg_integer()) ->
%%   {ok, Packet} | {error, Reason}

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
%%
%% @spec recv(connection_socket(), non_neg_integer(),
%%            non_neg_integer() | infinity) ->
%%   {ok, Packet} | {error, Reason}

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
%%
%% @spec controlling_process(socket(), pid()) ->
%%   ok | {error, Reason}

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
    error:Reason ->
      {error, Reason}
  end.

%% @doc Set socket options.
%%
%% @spec setopts(socket(), [option()]) ->
%%   ok | {error, Reason}

-spec setopts(socket(), [option()]) ->
  ok | {error, term()}.

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
%%
%% @spec close(socket()) ->
%%   ok

-spec close(socket()) ->
  ok.

close(Socket) when is_port(Socket) ->
  try
    unlink(Socket),
    port_close(Socket)
  catch
    % this could be caused by port already being closed, which is expected for
    % `{active,true}' sockets
    error:badarg -> ignore
  end,
  ok.

%%%---------------------------------------------------------------------------
%%% private helpers
%%%---------------------------------------------------------------------------

%% @doc Try accepting connection.
%%
%% @spec spawn_driver(listen | connect, string()) ->
%%   {ok, socket()} | {error, Reason}

spawn_driver(Type, Address) ->
  DriverCommand = case Type of
    listen  -> ?PORT_DRIVER_NAME ++ " l:" ++ Address;
    connect -> ?PORT_DRIVER_NAME ++ " c:" ++ Address
  end,
  ensure_driver_loaded(),
  try
    Port = open_port({spawn_driver, DriverCommand}, [binary]),
    {ok, Port}
  catch
    error:Reason ->
      {error, Reason}
  end.

%% @doc Try accepting connection.
%%
%% @spec try_accept(server_socket()) ->
%%   {ok, connection_socket()} | nothing

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

%% @doc Ensure the port driver library is loaded.
%%
%% @spec ensure_driver_loaded() ->
%%   ok

ensure_driver_loaded() ->
  PrivDir = code:lib_dir(?APP_NAME, priv),
  ok = erl_ddll:load_driver(PrivDir, ?PORT_DRIVER_NAME),
  ok.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
