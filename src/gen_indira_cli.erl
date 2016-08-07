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
%%%   line arguments. {@link indira:execute/3} facilitates this in `escript'
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
%  case indira:execute(Args, example_cli, Defaults) of
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
%%%   administrative connection ({@link gen_indira_listener}), though the
%%%   details may vary (e.g. reaction to refused connections).
%%%
%%%   The single distinguished operation is to start the application that is
%%%   the core of a daemon. This may be done using {@link application:start/2}
%%%   or {@link indira:start_rec/2} (though one needs to remember to configure
%%%   and start Indira). There are also functions {@link daemonize/2} and
%%%   {@link daemonize/5}, which simplify a little starting necessary
%%%   applications.
%%%
%%%   Starting a daemon could look like this:
%%%
%```
%-module(example_cli).
%-behaviour(gen_indira_cli).
%...
%
%handle_command(start = _Command, Options) ->
%  PidFile = get_pidfile(Options),
%  AdminSocket = get_admin_socket(Options),
%  NetConfig = get_net_config(Options),
%  case configure_application(Options) of
%    ok ->
%      indira:daemonize(example_app, {example_command_handler, []},
%                       [{indira_unix, AdminSocket}], PidFile, NetConfig);
%    {error, Reason} ->
%      {error, Reason}
%  end.
%
%parse_arguments(Args, [AdminSocket, PidFile, ConfigFile] = _Defaults) ->
%  case indira:args_folds(...) of
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
%%%             {@link indira:execute/3}</li>
%%%       </ul>
%%%
%%%       Arguments:
%%%       <ul>
%%%         <li>`Args' ({@type [string()]}) -- arguments passed in command
%%%             line</li>
%%%         <li>`DefaultValues' ({@type term()}) -- arbitrary term passed to
%%%             {@link indira:execute/3} that mainly allows move hardcoded
%%%             paths from module to `escript' script</li>
%%%       </ul>
%%%     </li>
%%%     <li>`handle_command(Command, Options)' -- execute a command, for which
%%%         simple track of sending a request with {@link
%%%         indira:send_one_command/3} and processing reply is not enough
%%%         (e.g. starting the daemon itself); function returns {@type ok} or
%%%         {@type @{error, term()@}}
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
%%%         or {@type @{error, term()@}}
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
%%%         @{error, term()@}}
%%%
%%%       Arguments:
%%%       <ul>
%%%         <li>`Reply' ({@type reply()}) -- reply to `Command' received from
%%%             daemon</li>
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
%%% @see indira:execute/3
%%% @see indira:daemonize/2
%%% @see indira:daemonize/5
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_cli).

-export([execute/3, daemonize/5, daemonize/2]).

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

-type socket_address() :: {module(), term()}.
%% A {@link gen_indira_listener} module and an address description for it (to
%% use with `send_one_line()' and `retry_send_one_line()').

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

%% @doc Execute an operation specified in command line.

-spec execute([string()], module(), term()) ->
    ok
  | help
  | {error, bad_request_format | bad_reply_format
            | {arguments | send | format, term()}
            | term()}.

execute(Args, CLIHandler, Defaults) ->
  case CLIHandler:parse_arguments(Args, Defaults) of
    {ok, Command, Options} ->
      execute_command(CLIHandler, Command, Options);
    {send, {_SockMod, _SockAddr} = Address, Command, Options} ->
      send_command(CLIHandler, Command, Options, Address);
    help ->
      help;
    {error, Reason} ->
      {error, {arguments, Reason}}
  end.

%%----------------------------------------------------------
%% execute actions returned by parse_arguments() {{{

%% @doc Pass a complex command to CLI module for execution.

-spec execute_command(module(), command(), options()) ->
  ok | {error, term()}.

execute_command(CLIHandler, Command, Options) ->
  CLIHandler:handle_command(Command, Options).

%% @doc Send a simple command to daemon and pass the reply to CLI module.

-spec send_command(module(), command(), options(), socket_address()) ->
  ok | {error, Reason}
when Reason :: bad_request_format
             | bad_reply_format
             | {send, term()}
             | {format, term()}.

send_command(CLIHandler, Command, Options, {SockMod, SockAddr} = _Address) ->
  case CLIHandler:format_request(Command, Options) of
    {ok, Request} ->
      case indira:send_one_command(SockMod, SockAddr, Request) of
        {ok, Reply} ->
          CLIHandler:handle_reply(Reply, Command, Options);
        {error, badarg} ->
          {error, bad_request_format};
        {error, bad_reply} ->
          {error, bad_reply_format};
        {error, Reason} ->
          {error, {send, Reason}}
      end;
    {error, Reason} ->
      {error, {format, Reason}}
  end.

%% }}}
%%----------------------------------------------------------

%% @doc Start the main application of the daemon.
%%
%% Function never returns, causing the calling process to sleep forever.
%%
%% Function also starts Indira with administrative command handler.
%%
%% Before running this function caller needs to set <i>indira/listen</i>
%% environment variable ({@link application:set_env/3}).

-spec daemonize(atom(), {module(), term()}) ->
  no_return() | {error, term()}.

daemonize(App, {CommMod, Args} = _CommModArgs) ->
  indira:set_option(indira, command, {CommMod, Args}),
  % TODO: handle errors
  ok = indira:start_rec(indira),
  ok = indira:start_rec(App),
  indira:sleep_forever(). % never return

%% @doc Start the main application of the daemon.
%%
%% Function sets <i>indira/listen</i>, <i>indira/pidfile</i>, and
%% <i>indira/net</i>, then calls {@link daemonize/2}.
%%
%% Function never returns, causing the calling process to sleep forever.

-spec daemonize(atom(), {module(), term()}, [{module(), term()}],
                file:filename() | undefined, tuple() | undefined) ->
  no_return() | {error, term()}.

daemonize(App, CommModArgs, ListenSpecs, PidFile, NetConfig) ->
  set_listen_specs(ListenSpecs),
  set_pidfile(PidFile),
  set_netconfig(NetConfig),
  daemonize(App, CommModArgs).

%% @doc Set <i>indira/listen</i> option.

set_listen_specs(ListenSpecs) ->
  indira:set_option(indira, listen, ListenSpecs).

%% @doc Set <i>indira/pidfile</i> option.

set_pidfile(undefined = _PidFile) ->
  ok;
set_pidfile(PidFile) ->
  indira:set_option(indira, pidfile, PidFile).

%% @doc Set <i>indira/net</i> option.
%%
%% @todo This is a placeholder that needs implementing.

set_netconfig(_NetConfig) ->
  'TODO'.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
