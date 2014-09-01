%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira listener (TCP, SSL, UNIX, you name it) entry point.
%%%
%%%   Module implementing this behaviour is an entry point for Indira to spawn
%%%   a listener, either directly a worker (e.g. for connection-less
%%%   protocols) or a supervision subtree (e.g. for TCP).
%%%
%%%   Note that it will be a good idea to describe what's expected as
%%%   the parameter in listener definition in Indira's config (the config
%%%   retrieved with `application:get_env(indira, listen)').
%%%
%%%   == Communication command executor ==
%%%
%%%   Command executor is a separate process, possibly implementing
%%%   `gen_server' behaviour. Messages sent to and expected from are
%%%   documented in {@link indira} module, as it's mostly seen by Indira's
%%%   user (daemon author).
%%%
%%%   == Communication with listener ==
%%%
%%%   Listener is expected to read a single line of the request from client
%%%   and send it using {@link command/2} or {@link command/3} to Indira
%%%   router. Indira router will parse it and send it further to command
%%%   center. Reply to the command will come as a message of form
%%%   `{result,Line}' (when {@link command/2} was called) or
%%%   `{result,RoutingHint,Line}' ({@link command/3}). You may depend on this
%%%   behaviour. `Line' <em>will not</em> include newline character in any
%%%   form.
%%%
%%%   {@link command/3} with `{result,RoutingHint,Line}' message are intended
%%%   for cases when single listener handles multiple clients and needs to
%%%   distinguish them. Listener may specify anything as a `RoutingHint' --
%%%   it's opaque to Indira router.
%%%
%%%   To provide uniformly formatted logs, listener should log errors using
%%%   {@link log_error/3} (e.g. in case of problems in communication with
%%%   client) or {@link log_critical/3} (e.g. in case of socket setup error).
%%%
%%%   === Line format ===
%%%
%%%   Formats of the request and response lines are documented in
%%%   {@link indira} module.
%%%
%%%   == Entry point module API ==
%%%
%%%   `Module:supervision_child_spec/2' gets two arguments: Indira router
%%%   address suitable for {@link command/2} and the term that was specified
%%%   as module argument in environment specification. Now, `Module' has an
%%%   opportunity to pass Indira handle to the child to be spawned.
%%%
%%%   `Module:supervision_child_spec/2' is supposed to return `{MFA, Type}',
%%%   where `Type' is `worker' or `supervisor' and `MFA' is `{Module,
%%%   Function, Args}' suitable for {@link erlang:apply/3}. Semantics are the
%%%   same as `StartFunc' in child specification in supervisor.
%%%
%%%   Typically, for module `foo' it would be:
%%%   <ul>
%%%     <li>`{{foo, start_link, []}, worker}' for `foo:start_link/0' that runs
%%%         worker</li>
%%%     <li>`{{foo_sup, start_link, []}, supervisor}' for
%%%         `foo_sup:start_link/0' that runs whole supervision tree</li>
%%%   </ul>
%%%   Note that the definitions above don't pass Indira router address to
%%%   spawned processes. The address most probably should be passed in `Args'
%%%   (which equals to `[]' in above examples).
%%%
%%%   @TODO Change `supervision_child_spec/2' to `child_spec/2' and return
%%%     full child specification, not just fragments.
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_listener).

-export([behaviour_info/1]).

%% API for listeners
-export([command/2, command/3]).
-export([log_info/2, log_error/2, log_error/3,
         log_critical/2, log_critical/3]).

%%%---------------------------------------------------------------------------

%% @doc Behaviour description.
behaviour_info(callbacks = _Aspect) ->
  [{supervision_child_spec, 2}];
behaviour_info(_Aspect) ->
  undefined.

%%%---------------------------------------------------------------------------
%%% API for listeners
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% send command line to router {{{

%% @doc Send command to Indira router.
%%   According to client protocol, response to the command from executor will
%%   be returned as a message to the process that called this function.
%%
%%   This function is intended to be called from {@link gen_indira_listener}
%%   supervision tree.
%%
%% @see indira_router:command/2
command(Indira, Line) ->
  indira_router:command(Indira, Line).

%% @doc Send command to Indira router.
%%   The process calling this function will get the response as a message.
%%
%%   `RoutingKey' is an additional information to tell apart between multiple
%%   clients and will be included in command reply message.
%%
%%   This call form is only needed when a single process handles multiple
%%   clients.
%%
%%   This function is intended to be called from {@link gen_indira_listener}
%%   supervision tree.
%%
%% @see indira_router:command/3
command(Indira, RoutingKey, Line) ->
  indira_router:command(Indira, RoutingKey, Line).

%% }}}
%%----------------------------------------------------------
%% logging (unified) {{{

%% @doc Send info report about an event to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
log_info(InfoType, Context) ->
  error_logger:info_report([{indira_info, InfoType} | Context]).

%% @doc Send error report about an error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use with single `{error,Reason}' tuple.
%%
%%   Such error is something that generally shouldn't happen, but if it does,
%%   it has limited scope (e.g. command line parse error for a single client).
log_error(ErrorType, ErrorReason, Context) ->
  error_logger:warning_report(
    [{indira_error, ErrorType}, {error, ErrorReason} | Context]
  ).

%% @doc Send error report about an error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use when no single `{error,Reason}' tuple
%%   is present (e.g. when multiple errors occurred).
%%
%%   Such error is something that generally shouldn't happen, but if it does,
%%   it has limited scope (e.g. command line parse error for a single client).
log_error(ErrorType, Context) ->
  error_logger:warning_report([{indira_error, ErrorType} | Context]).

%% @doc Send log report about severe error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use with single `{error,Reason}' tuple.
%%
%%   For severe errors, like inability to listen on a specified port.
log_critical(ErrorType, ErrorReason, Context) ->
  error_logger:error_report(
    [{indira_error, ErrorType}, {error, ErrorReason} | Context]
  ).

%% @doc Send log report about severe error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use when no single `{error,Reason}' tuple
%%   is present (e.g. when multiple errors occurred).
%%
%%   For severe errors, like inability to listen on a specified port.
log_critical(ErrorType, Context) ->
  error_logger:error_report([{indira_error, ErrorType} | Context]).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
