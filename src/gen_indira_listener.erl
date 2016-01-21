%%%---------------------------------------------------------------------------
%%% @doc
%%%   Behaviour for socket listener entry point.
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
%%%   Result comes as a message of form either
%%%   {@type @{result, ReplyLine :: iolist()@}} (when `command(Line)' was
%%%   called) or {@type @{result, Hint :: term(), ReplyLine :: iolist()@}}
%%%   (when `command(Hint, Line)' was called). The latter case is intended for
%%%   cases when a single process is responsible for communication with all
%%%   the clients, and `Hint' allows to determine to which client send the
%%%   `ReplyLine'.
%%%
%%%   To provide uniformly formatted logs, listener should log errors using
%%%   {@link indira_log:error/3} (e.g. in case of problems in communication
%%%   with client) or {@link indira_log:critical/3} (e.g. in case of socket
%%%   setup error).
%%%
%%%   == Entry point module API ==
%%%
%%%   Module implementing this behaviour needs to have following functions
%%%   exported:
%%%
%%%   <ul>
%%%     <li>`child_spec(ListenAddress) -> ChildSpec' -- return the root of
%%%           supervision (sub)tree that accepts connections on
%%%           `ListenAddress'
%%%       <ul>
%%%         <li>`ListenAddress' ({@type term()}) -- an arbitrary term that
%%%             describes address to accept client connections; the same term
%%%             as passed in environment specification</li>
%%%         <li>`ChildSpec' ({@type supervisor:child_spec()}) -- supervision
%%%             specification of top-level process for this tree (may be
%%%             a single worker, like {@link indira_udp} does, or a whole
%%%             supervisor, similar to {@link indira_tcp} or {@link
%%%             indira_unix})</li>
%%%       </ul>
%%%     </li>
%%%   </ul>
%%%
%%%   Several fields of what `child_spec(Addr)' returns are ignored. Assuming
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
%%%   Typically, for module `foo' it could be:
%%%   <ul>
%%%     <li>`{ignore, {foo, start_link, []}, permanent, 5000, worker, [foo]}'
%%%         for `foo:start_link/0' that runs worker</li>
%%%     <li>`{ignore, {foo_sup, start_link, []}, permanent, 5000, supervisor,
%%%         [foo_sup]}' for `foo_sup:start_link/0' that runs whole supervision
%%%         tree</li>
%%%   </ul>
%%%
%%% @see indira_log
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_listener).

%% sending commands to router
-export([command/1, command/2]).

%%%---------------------------------------------------------------------------

-callback child_spec(ListenAddress :: term()) ->
  supervisor:child_spec().

%%%---------------------------------------------------------------------------
%%% sending commands to router
%%%---------------------------------------------------------------------------

%% @doc Send command to Indira router.
%%
%%   Calling process will later receive a message
%%   {@type @{result, ReplyLine :: iolist()@}}, with `ReplyLine' <em>not
%%   including</em> terminating newline character.
%%
%%   This function is intended to be called from {@link gen_indira_listener}
%%   supervision tree.

-spec command(string() | binary()) ->
  ok.

command(Line) ->
  indira_commander:command(Line).

%% @doc Send command to Indira router.
%%   The process calling this function will get the response as a message.
%%
%%   Calling process will later receive a message
%%   {@type @{result, RoutingKey, ReplyLine :: iolist()@}}, with `ReplyLine'
%%   <em>not including</em> terminating newline character, and `RoutingKey'
%%   being the same as specified in the argument to this function.
%%
%%   `RoutingKey' is an additional information to tell apart between multiple
%%   clients. This call form is only needed when a single process handles
%%   multiple clients.
%%
%%   This function is intended to be called from {@link gen_indira_listener}
%%   supervision tree.

-spec command(term(), string() | binary()) ->
  ok.

command(RoutingKey, Line) ->
  indira_commander:command(RoutingKey, Line).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
