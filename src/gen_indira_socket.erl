%%%---------------------------------------------------------------------------
%%% @doc
%%%   Behaviour for socket listener entry point.
%%%
%%%   This module is intended for people who want to implement their own
%%%   administrative sockets, similar to {@link indira_unix},
%%%   {@link indira_tcp}, and {@link indira_udp}.
%%%
%%%   Module implementing this behaviour is an entry point for Indira to spawn
%%%   a listener, either directly a worker (e.g. for connection-less
%%%   protocols) or a supervision subtree (e.g. for TCP).
%%%
%%%   Note that it will be a good idea to describe what's expected as
%%%   the parameter in listener definition in Indira's config (the config
%%%   retrieved with `application:get_env(indira, listen)').
%%%
%%%   == Reading and executing commands from client ==
%%%
%%%   After reading a line from a client, listener (or worker on its behalf)
%%%   is supposed to call {@link command/1} or {@link command/2} to send the
%%%   line to parsing and execution, and then to have a reply sent back.
%%%
%%%   Listener is not involved with serialization or deserialization of
%%%   messages in any way.
%%%
%%%   Result comes as a message of one of several forms. When `command(Line)'
%%%   was called, it will be either {@type @{result, ReplyLine :: iolist()@}}
%%%   {@type @{error, throw, Reason :: term()@}}, or {@type @{error, exit
%%%   | error, Reason :: term(), StackTrace :: list()@}}. When `command(Line,
%%%   Hint)' was called, it will be {@type @{result, Hint, ReplyLine ::
%%%   iolist()@}}, {@type @{error, Hint, throw, Reason :: term()@}}, or
%%%   {@type @{error, Hint, exit | error | throw, Reason :: term(),
%%%   StackTrace :: list()@}}. `ReplyLine' does not include trailing newline
%%%   character. `StackTrace' is a result of {@link erlang:get_stacktrace/0}
%%%   call.
%%%
%%%   The first three messages are intended for cases when each client
%%%   connection is handled by its own process (as do {@link indira_tcp} and
%%%   {@link indira_unix}). The latter three are handy when a single process
%%%   is responsible for all the communication on the socket and need some
%%%   means to tell where to send the reply (e.g. `Hint' can be `{IP,Port}',
%%%   as {@link indira_udp} does).
%%%
%%%   To provide uniformly formatted logs, listener should log errors using
%%%   {@link indira_log:warn/3} (e.g. in case of problems in communication
%%%   with client) or {@link indira_log:crit/3} (e.g. in case of socket setup
%%%   error).
%%%
%%%   == Entry point module API ==
%%%
%%%   Module implementing this behaviour needs to have following functions
%%%   exported:
%%%
%%%   <ul>
%%%     <li>`child_spec(Address)' -- how to start a listener process to accept
%%%         connections on {@type Address :: listen_address()}; function
%%%         should return {@type supervisor:child_spec()}
%%%     </li>
%%%     <li>`send_one_line(Address, Line, Timeout)' -- send a line to an
%%%         administrative socket; function should return
%%%         {@type @{ok, ReplyLine :: iolist()@}} (`ReplyLine' may include
%%%         trailing newline) or {@type @{error, error()@}}
%%%       <ul>
%%%         <li>`Address' ({@type connect_address()}) -- address to send
%%%             command to</li>
%%%         <li>`Line' ({@type iolist()}) -- line with serialized command
%%%             (trailing newline <em>not included</em>)</li>
%%%         <li>`Timeout' ({@type timeout()}) -- how long to wait for reply
%%%             (milliseconds or `infinity')</li>
%%%       </ul>
%%%     </li>
%%%     <li>`retry_send_one_line(Address, Line, Timeout)' -- send a line to an
%%%         administrative socket, retrying when connection was refused;
%%%         function should return {@type @{ok, ReplyLine :: iolist()@}}
%%%         (`ReplyLine' may include trailing newline) or
%%%         {@type @{error, error()@}}
%%%       <ul>
%%%         <li>`Address' ({@type connect_address()}) -- address to send
%%%             command to</li>
%%%         <li>`Line' ({@type iolist()}) -- line with serialized command
%%%             (trailing newline <em>not included</em>)</li>
%%%         <li>`Timeout' ({@type timeout()}) -- how long to wait for reply
%%%             (milliseconds or `infinity')</li>
%%%       </ul>
%%%     </li>
%%%     <li>`format_error(Reason)' -- make a printable message from an error
%%%         returned from a function from the module
%%%       <ul>
%%%         <li>`Reason' ({@type error()}) -- second element of an error tuple
%%%             (`{error,Reason}')</li>
%%%       </ul>
%%%     </li>
%%%   </ul>
%%%
%%%   Some fields of what `child_spec(Address)' returns are ignored. Assuming
%%%   that the result ({@type supervisor:child_spec()}) is matched against
%%%   tuple `{Id, MFA, Restart, Shutdown, Type, Modules}':
%%%   <ul>
%%%     <li>`Id' is ignored</li>
%%%     <li>`MFA' is `{Module, Function, Args}' suitable for
%%%         {@link erlang:apply/3}</li>
%%%     <li>`Restart' is ignored and will always be set to `permanent'</li>
%%%     <li>`Shutdown' is `brutal_kill', integer >0 or `infinity'</li>
%%%     <li>`Type' is `worker' or `supervisor'</li>
%%%     <li>`Modules' is a list of modules used by the child or a single atom
%%%         `dynamic'</li>
%%%   </ul>
%%%
%%% @see indira_log
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_socket).

%% sending commands to router
-export([command/1, command/2]).
%% timers
-export([setup_timer/1, cancel_timer/1, timer_fired/2]).

-export_type([address/0, listen_address/0, connect_address/0]).
-export_type([timer/0]).
-export_type([error/0]).

%%%---------------------------------------------------------------------------

-type timer() :: {reference(), reference()}.

-type address() :: term().

-type listen_address() :: address().

-type connect_address() :: address().

-type error() :: term().

%%%---------------------------------------------------------------------------

-callback child_spec(Address :: listen_address()) ->
  supervisor:child_spec().

-callback send_one_line(Address :: connect_address(), Line :: iolist(),
                        Timeout :: timeout()) ->
  {ok, ReplyLine :: iolist()} | {error, term()}.

-callback retry_send_one_line(Address :: connect_address(), Line :: iolist(),
                              Timeout :: timeout()) ->
  {ok, ReplyLine :: iolist()} | {error, term()}.

-callback format_error(Reason :: error()) ->
  iolist().

%%%---------------------------------------------------------------------------
%%% executing commands
%%%---------------------------------------------------------------------------

%% @doc Spawn a command execution process.
%%
%%   Calling process will later receive a message
%%   {@type @{result, ReplyLine :: iolist()@}}, with `ReplyLine' <em>not
%%   including</em> terminating newline character.
%%
%%   If the command handler died or returned an unserializable value, message
%%   {@type @{error, throw, Reason :: term()@}} or
%%   {@type @{error, exit | error, Reason :: term(), StackTrace :: list()@}}
%%   will be sent (`StackTrace' is documented in {@link
%%   erlang:get_stacktrace/0}).

-spec command(string() | binary()) ->
  ok.

command(Line) ->
  {ok, Pid} = indira_command:spawn_worker(),
  indira_command:execute(Pid, Line).

%% @doc Spawn a command execution process.
%%
%%   Calling process will later receive a message
%%   {@type @{result, RoutingKey, ReplyLine :: iolist()@}}, with `ReplyLine'
%%   <em>not including</em> terminating newline character, and `RoutingKey'
%%   being the same as specified in the argument to this function.
%%
%%   If the command handler died or returned an unserializable value,
%%   message {@type @{error, RoutingKey, throw, Reason :: term()@}} or
%%   {@type @{error, RoutingKey, exit | error, Reason :: term(),
%%   StackTrace :: list()@}} will be sent (`StackTrace' is documented in
%%   {@link erlang:get_stacktrace/0}).
%%
%%   `RoutingKey' is an additional information to tell apart between multiple
%%   clients. This call form is only needed when a single process handles
%%   multiple clients, like {@link indira_udp} does.

-spec command(string() | binary(), term()) ->
  ok.

command(Line, RoutingKey) ->
  {ok, Pid} = indira_command:spawn_worker(),
  indira_command:execute(Pid, Line, RoutingKey).

%%%---------------------------------------------------------------------------
%%% timers (for implementing retry_send_one_line())
%%%---------------------------------------------------------------------------

%% @doc Setup a timer to fire after `Timeout' milliseconds.
%%   Handy for implementing `retry_send_one_line()'.

-spec setup_timer(timeout()) ->
  timer().

setup_timer(Timeout) ->
  MsgRef = make_ref(),
  TimerRef = erlang:send_after(Timeout, self(), {timeout, MsgRef}),
  {MsgRef, TimerRef}.

%% @doc Cancel a timer set up with {@link setup_timer/1}.
%%   Handy for implementing `retry_send_one_line()'.

-spec cancel_timer(timer()) ->
  ok.

cancel_timer({_MsgRef, TimerRef} = Timer) ->
  erlang:cancel_timer(TimerRef),
  timer_fired(Timer, 0), % flush, ignore the result
  ok.

%% @doc Check if timer fired or will fire within `Timeout' milliseconds.
%%   Handy for implementing `retry_send_one_line()'.

-spec timer_fired(timer(), timeout()) ->
  boolean().

timer_fired({MsgRef, _TimerRef} = _Timer, Timeout) ->
  receive
    {timeout, MsgRef} ->
      true
  after Timeout ->
      false
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
