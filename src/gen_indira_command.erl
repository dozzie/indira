%%%---------------------------------------------------------------------------
%%% @doc
%%%   Administrative command handler behaviour.
%%%
%%%   Module implementing this behaviour will be called to handle any
%%%   administrative command sent to Indira.
%%%
%%%   Each command is handled in a separate Erlang process, so it can take as
%%%   long as necessary to finish.
%%%
%%%   To specify command handler, set <i>command</i> environment for
%%%   application <i>indira</i> (`application:set_env(indira, command,
%%%   Callback)') to `Callback' of this type:
%%%   {@type @{module(), term()@} | fun() | @{fun(), term()@}}. The additional
%%%   {@type term()} is an arbitrary hint to be passed as-is as the second
%%%   argument to call.
%%%
%%%   == Command handler as a module ==
%%%
%%%   Module implementing this behaviour needs to have following functions
%%%   exported:
%%%
%%%   <ul>
%%%     <li>`handle_command(Command, Arg) -> Response' -- handle a command
%%%         received through one of {@link gen_indira_socket} channels:
%%%       <ul>
%%%         <li>`Command' ({@type command()}) -- command to be handled</li>
%%%         <li>`Arg' ({@type argument()}) -- an arbitrary hint specified by
%%%             Indira's user to parametrize the callback module</li>
%%%         <li>`Response' ({@type command_response()}) -- response to be sent
%%%             back</li>
%%%       </ul>
%%%     </li>
%%%   </ul>
%%%
%%%   === Other useful functions to define ===
%%%
%%%   Since the module implementing {@link gen_indira_command} behaviour will
%%%   receive commands and produce results all packed into a
%%%   {@link indira_json:struct(). jsx-like structure}, it only makes sense to
%%%   put functions preparing and unpacking such structures for each command
%%%   and result in the same module. See {@link examples_command_handler} for
%%%   example module.
%%%
%%%   == Command handler as a function ==
%%%
%%%   Instead of specifying a handler module, you can provide just a function.
%%%   It may be handy if you want to keep everything in controller script.
%%%
%%%   Note that it won't be easy to reload command handler's code, so it's
%%%   generally recommended to use the handler module approach.
%%%
%%%   To specify a function handler, you provide a single argument function
%%%   or a tuple with two-argument function and an arbitrary term:
%%%
%%%   <ul>
%%%     <li>`fun((Command) -> Response)'
%%%       <ul>
%%%         <li>`Command' ({@type command()}) -- command to be handled</li>
%%%         <li>`Arg' ({@type argument()}) -- an arbitrary hint specified by
%%%             Indira's user to parametrize the callback module</li>
%%%         <li>`Response' ({@type command_response()}) -- response to be sent
%%%             back</li>
%%%       </ul>
%%%     </li>
%%%     <li>`{Function, Arg}'
%%%       <ul>
%%%         <li>`Function' is defined as `fun((Command, Arg) -> Response)'</li>
%%%         <li>`Command' ({@type command()}) -- command to be handled</li>
%%%         <li>`Arg' ({@type argument()}) -- an arbitrary hint specified by
%%%             Indira's user to parametrize the callback module</li>
%%%         <li>`Response' ({@type command_response()}) -- response to be sent
%%%             back</li>
%%%       </ul>
%%%     </li>
%%%   </ul>
%%%
%%%   == Useful commands to handle ==
%%%
%%%   Given the nature of daemons, it could be useful to implement some of the
%%%   following commands:
%%%
%%%   <ul>
%%%     <li><i>stop</i>, shutdown the daemon (usually called from
%%%         initscript) (see {@link init:stop/0})</li>
%%%     <li><i>status</i>, check if the daemon is running, with an option to
%%%         wait for boot process to finish (called from initscript)</li>
%%%     <li><i>reload</i>, reload configuration stored in a file</li>
%%%     <li><i>netconfig</i>, setup distributed Erlang (epmd, cookie and
%%%         <i>-(s)name</i>), so an administrator can get shell and debug
%%%         daemon's internals (see {@link
%%%         indira_app:distributed_start/0})</li>
%%%     <li><i>increase/decrease logging level</i>, for administrator to
%%%         diagnose operations without resorting to tracing Erlang</li>
%%%   </ul>
%%%
%%%   The list is by no means exhaustive.
%%%
%%% @see indira_json
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_command).

-export_type([command/0, command_response/0, argument/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

-type command() :: indira_json:struct().
%% Command received from one of the {@link gen_indira_socket} channels.

-type command_response() :: indira_json:struct().
%% Response to a {@type command()}.

-type argument() :: term().
%% An arbitrary hint to `handle_command/2' function.

%%% }}}
%%%---------------------------------------------------------------------------

-callback handle_command(Command :: command(), Arg :: argument()) ->
  command_response().

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
