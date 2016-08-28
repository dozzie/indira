%%%---------------------------------------------------------------------------
%%% @doc
%%%   Command line arguments handler.
%%%
%%%   Module implementing this behaviour will be used for parsing arguments
%%%   passed in command line, decoding from them an operation to be performed
%%%   (usually starting daemon and sending it one of supported administrative
%%%   commands), and executing it.
%%%
%%%   == Expected typical usage ==
%%%
%%%   === command line script ===
%%%
%%%   `gen_indira_cli' behaviour is intended to structure processing command
%%%   line arguments. {@link indira_cli:execute/3} facilitates this in `escript'
%%%   scripts, (`escript' is a part of runtime that is well-suited for this
%%%   task, but not the only one). Such a script could look like this:
%%%
%```
%#!/usr/bin/escript
%
%main(Args) ->
%  AdminSocket = "/var/run/example_app/control",
%  PidFile     = "/var/run/example_app/pid",
%  ConfigFile  = "/etc/example_app.conf",
%  Defaults = [AdminSocket, PidFile, ConfigFile],
%  case indira_cli:execute(Args, example_cli, Defaults) of
%    ok ->
%      ok;
%    help ->
%      io:fwrite("~s~n", [example_cli:usage()]);
%    {error, {arguments, Reason}} ->
%      io:fwrite(standard_error, "~p~n", [Reason]), % not a pretty message
%      io:fwrite(standard_error, "~s~n", [example_cli:usage()]);
%      halt(1);
%    {error, Reason} ->
%      io:fwrite(standard_error, "~p~n", [Reason]), % not a pretty message
%      halt(1)
%  end.
%'''
%%%
%%%   === command line handler module ===
%%%
%%%   The other side is a module that contains operations' code, i.e. the one
%%%   implementing `gen_indira_cli' behaviour.
%%%
%%%   Most of the commands are expected to be sent to daemon instance through
%%%   administrative connection ({@link gen_indira_socket}), though the
%%%   details may vary (e.g. reaction to refused connections).
%%%
%%%   The single distinguished operation is to start the application that is
%%%   the core of a daemon. This may be done using {@link application:start/2}
%%%   or {@link indira_app:start_rec/2} (though one needs to remember to
%%%   configure and start Indira). There is also
%%%   {@link indira_app:daemonize/2}, which simplifies starting necessary
%%%   applications a little.
%%%
%%%   Starting a daemon could look like this:
%%%
%```
%-module(example_cli).
%-behaviour(gen_indira_cli).
%...
%
%handle_command(start = _Command, Options) ->
%  AdminSocket = get_admin_socket(Options),
%  case configure_application(Options) of
%    ok ->
%      indira_app:daemonize(example_app, [
%        {listen, [{indira_unix, AdminSocket}]},
%        {command, {example_command_handler, []}}
%      ]);
%    {error, Reason} ->
%      {error, Reason}
%  end.
%
%parse_arguments(Args, [AdminSocket, PidFile, ConfigFile] = _Defaults) ->
%  case indira_cli:folds(...) of
%    {ok, {start, Options}} -> {ok, start, Options};
%    ...
%  end.
%'''
%%%
%%%   == Expected callbacks ==
%%%
%%%   <ul>
%%%     <li>`parse_arguments(Args, DefaultValues)' -- determine what operation
%%%         to execute from arguments passed in command line
%%%
%%%       Returned value:
%%%       <ul>
%%%         <li>{@type @{ok, command(), options()@}} -- execute a more complex
%%%             command by calling `handle_command()'</li>
%%%         <li>{@type @{send, socket_address(), command(), options()@}} --
%%%             execute a simple command by calling `format_request()',
%%%             sending returned request through administrative socket, and
%%%             calling `handle_reply()' on the reply</li>
%%%         <li>{@type help} -- print a help message to screen (e.g.
%%%             <i>--help</i> option was provided)</li>
%%%         <li>{@type @{error, Reason :: term()@}} -- signal an erroneous
%%%             command line; returned as `{error, {arguments, Reason}}' from
%%%             {@link indira_cli:execute/3}</li>
%%%       </ul>
%%%
%%%       Arguments:
%%%       <ul>
%%%         <li>`Args' ({@type [string()]}) -- arguments passed in command
%%%             line</li>
%%%         <li>`DefaultValues' ({@type term()}) -- arbitrary term passed to
%%%             {@link indira_cli:execute/3} that mainly allows move hardcoded
%%%             paths from module to `escript' script</li>
%%%       </ul>
%%%     </li>
%%%     <li>`handle_command(Command, Options)' -- execute a command, for which
%%%         simple track of sending a request with {@link
%%%         indira_cli:send_one_command/4} and processing reply is not enough
%%%         (e.g. starting the daemon itself); function returns {@type ok} or
%%%         {@type @{error, term()@}} (returned verbatim to {@link execute/3}
%%%         caller)
%%%
%%%       Arguments:
%%%       <ul>
%%%         <li>`Command' ({@type command()}) -- command to be executed</li>
%%%         <li>`Options' ({@type options()}) -- options set in command
%%%             line</li>
%%%       </ul>
%%%     </li>
%%%     <li>`format_request(Command, Options)' -- encode a command as
%%%         a JSON-serializable structure, so it can be sent through
%%%         administrative socket; function returns {@type @{ok, request()@}}
%%%         or {@type @{error, Reason :: term()@}} (returned as `{error,
%%%         {format, Reason}}' to {@link execute/3} caller)
%%%
%%%       Arguments:
%%%       <ul>
%%%         <li>`Command' ({@type command()}) -- command to be sent to
%%%             daemon</li>
%%%         <li>`Options' ({@type options()}) -- options set in command
%%%             line</li>
%%%       </ul>
%%%     </li>
%%%     <li>`handle_reply(Reply, Command, Options)' -- process a reply to
%%%         a command sent to daemon; function returns {@type ok} or {@type
%%%         @{error, term()@}} (returned verbatim to {@link execute/3} caller)
%%%
%%%       Arguments:
%%%       <ul>
%%%         <li>`Reply' ({@type reply()}) -- reply to `Command' received from
%%%             daemon; all hashes in `Reply' are compatible with {@link
%%%             orddict} module</li>
%%%         <li>`Command' ({@type command()}) -- command (the original one,
%%%             returned from `parse_arguments()') that was sent to
%%%             daemon</li>
%%%         <li>`Options' ({@type options()}) -- options set in command
%%%             line</li>
%%%       </ul>
%%%     </li>
%%%   </ul>
%%%
%%% @see gen_indira_command
%%% @see indira_cli:execute/3
%%% @see indira_app:daemonize/2
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_cli).

-export_type([command/0, options/0, request/0, reply/0, socket_address/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

-type command() :: term().
%% Command to execute. Usually an atom is enough to describe what to do.

-type options() :: term().
%% Options that change details of the {@type command()}, like administrative
%% socket location, config file, etc.

-type request() :: indira_json:struct().
%% {@type command()} encoded as a JSON-serializable structure, ready to be
%% sent to daemon instance. See also {@link indira_json}.

-type reply() :: indira_json:struct().
%% Reply received from daemon to an administrative command. See also {@link
%% indira_json}.

-type socket_address() :: {module(), gen_indira_socket:connect_address()}.
%% A {@link gen_indira_socket} module and an address for it to use with
%% `send_one_line()' and `retry_send_one_line()'.

%%% }}}
%%%---------------------------------------------------------------------------

-callback parse_arguments(Args :: [string()], Defaults :: term()) ->
    {ok, command(), options()}
  | {send, socket_address(), command(), options()}
  | help
  | {error, term()}.

-callback handle_command(Command :: command(), Options :: options()) ->
  ok | {error, term()}.

-callback format_request(Command :: command(), Options :: options()) ->
  {ok, request()} | {error, term()}.

-callback handle_reply(Reply :: reply(), Command :: command(),
                       Options :: options()) ->
  ok | {error, term()}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
