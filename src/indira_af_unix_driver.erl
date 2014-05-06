%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Module for cooperating with `AF_UNIX' port driver.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_af_unix_driver).

%% server socket handling
-export([listen/1, poll/1, poll/2, close/1]).
%% client socket handling
-export([send/3, close/2]).

-export_type([event/0]).

%%%---------------------------------------------------------------------------

-define(PORT_DRIVER_NAME, "af_unix_drv").
-define(APP_NAME, indira).

%%%---------------------------------------------------------------------------

%% @type event() = {new, FD} | {data, FD :: integer(), binary()} | {close, FD}.
%%   Activity event: new connection, connection closed by client or incoming
%%   data.

-type event() :: {new, FD} | {data, FD :: integer(), binary()} | {close, FD}.

%%%---------------------------------------------------------------------------
%%% server socket handling
%%%---------------------------------------------------------------------------

%% @doc Create server (listening) socket.
%%
%% @spec listen(string() | binary()) ->
%%   {ok, port()} | {error, Reason :: term()}

-spec listen(string() | binary()) ->
  {ok, port()} | {error, Reason :: term()}.

listen(Address) ->
  ensure_driver_loaded(),
  Port = open_port({spawn_driver, ?PORT_DRIVER_NAME}, [binary]),
  port_command(Port, ["o", Address]),
  receive
    {Port, {data, <<"+">>}} ->
      {ok, Port};
    {Port, {data, <<"!", Error/binary>>}} ->
      port_close(Port),
      {error, Error}
  end.

%% @doc Check for any pending activity on sockets (server and clients).
%%   Function does not wait for events, just returns what's already pending.
%%
%% @spec poll(port()) ->
%%   [event()]

-spec poll(port()) ->
  [event()].

poll(Port) ->
  poll(Port, 0).

%% @doc Check for any activity on sockets (server and clients).
%%   Function waits for `Timeout' miliseconds before returning.
%%
%%   <b>WARNING</b>: This function locks the whole Erlang VM. Better use
%%   {@link poll/1}.
%%
%% @spec poll(port(), integer()) ->
%%   [event()]

-spec poll(port(), integer()) ->
  [event()].

poll(Port, Timeout) ->
  port_command(Port, <<"p", Timeout:32>>),
  Events = receive_all(Port),
  Events.

%% @doc Close the server socket and all the associated client connections.
%%
%% @spec close(port()) ->
%%   ok

-spec close(port()) ->
  ok.

close(Port) ->
  port_close(Port),
  ok.

%%%---------------------------------------------------------------------------
%%% client socket handling
%%%---------------------------------------------------------------------------

%% @doc Send payload to down to client.
%%
%% @spec send(port(), integer(), iolist()) ->
%%   ok

send(Port, FD, Data) ->
  port_command(Port, [<<"d", FD:16>>, Data]),
  ok.

%% @doc Close client connection.
%%
%% @spec close(port(), integer()) ->
%%   ok

close(Port, FD) ->
  port_command(Port, <<"c", FD:16>>),
  ok.

%%%---------------------------------------------------------------------------
%%% helpers
%%%---------------------------------------------------------------------------

%% @doc Collect pending events from sockets.
%%
%% @spec receive_all(port()) ->
%%   [event()]

-spec receive_all(port()) ->
  [event()].

receive_all(Port) ->
  receive
    {Port, {data, <<"n", FD:16>>}} ->
      [{new, FD} | receive_all(Port)];
    {Port, {data, <<"d", FD:16, Data/binary>>}} ->
      [{data, FD, Data} | receive_all(Port)];
    {Port, {data, <<"c", FD:16>>}} ->
      [{close, FD} | receive_all(Port)];
    {Port, {data, <<"e">>}} ->
      []
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
