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
%  AdminSocket = get_admin_socket(Options),
%  case configure_application(Options) of
%    ok ->
%      indira:daemonize(example_app, [
%        {listen, [{indira_unix, AdminSocket}]},
%        {command, {example_command_handler, []}}
%      ]);
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
%%% @TODO Add {@type daemon_option()} to start other applications before daemon.
%%%
%%% @see gen_indira_command
%%% @see indira:execute/3
%%% @see indira:daemonize/2
%%% @see indira:daemonize/5
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_cli).

-export([execute/3, daemonize/2]).

-export_type([command/0, options/0, request/0, reply/0, socket_address/0]).
-export_type([daemon_option/0]).

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

-type daemon_option() ::
    {listen, [{module(), term()}]}
  | {command, {module(), term()}}
  | {pidfile, file:filename() | undefined}
  | {node_name, node() | undefined}
  | {name_type, shortnames | longnames | undefined}
  | {cookie, none | undefined | atom() | {file, file:filename()}}
  | {net_start, boolean() | undefined}
  | {start_before, AppName :: atom()}
  | {start_after, AppName :: atom()}.
%% Options that correspond to Indira's environment. `node_name', `name_type',
%% and `cookie' compose <i>indira/net</i> parameter. See {@link indira} for
%% details. `{start_*, AppName}' allow to start additional applications (e.g.
%% SASL) before or after Indira.

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
%%
%% Error formats:
%% <ul>
%%   <li>`{error, {arguments, Reason}' when `parse_arguments()' callback
%%       returns an error</li>
%%   <li>`{error, {format, Reason}}' when `format_request()' callback returns
%%       an error</li>
%%   <li>`{error, {send, Reason}}' on connection or (de)serialization error
%%       (the same as for {@link indira:send_one_command/3})</li>
%%   <li>`{error, Reason}' when `handle_command()' or `handle_reply()',
%%       whichever is called, return an error</li>
%% </ul>

-spec execute([string()], module(), term()) ->
  ok | help | {error, Reason}
  when Reason :: {arguments, term()}
               | {format, term()}
               | {send, bad_request_format | bad_reply_format | term()}
               | term().

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
  when Reason :: {send, bad_request_format | bad_reply_format | term()}
               | {format, term()}
               | term().

send_command(CLIHandler, Command, Options, {SockMod, SockAddr} = _Address) ->
  case CLIHandler:format_request(Command, Options) of
    {ok, Request} ->
      case indira:send_one_command(SockMod, SockAddr, Request) of
        {ok, Reply} ->
          CLIHandler:handle_reply(Reply, Command, Options);
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
%% Function sets all Indira's parameters specified in options (options
%% `{listen, [...]}' and `{command, {Mod,Args}}' are mandatory), and then
%% starts Indira and `App', in this order.
%%
%% NOTE: Setting an option to `undefined' has the same result as omitting it
%% altogether.
%%
%% Function never returns, causing the calling process to sleep forever.

-spec daemonize(atom(), [daemon_option()]) ->
  no_return() | {error, Reason}
  when Reason :: invalid_listen_spec | missing_listen_spec
               | invalid_command_handler | missing_command_handler
               | invalid_pidfile
               | invalid_net_config | invalid_net_start.

daemonize(App, Options) ->
  case set_indira_options(Options) of
    {ok, StartBefore, StartAfter} ->
      % TODO: handle errors
      ok = start_all_applications(StartBefore),
      ok = indira:start_rec(indira),
      ok = start_all_applications(StartAfter),
      ok = indira:start_rec(App),
      indira:sleep_forever(); % never return
    {error, Reason} ->
      {error, Reason}
  end.

%%----------------------------------------------------------
%% start a list of applications {{{

%% @doc Start all the specified applications.

-spec start_all_applications([atom()]) ->
  ok | {error, {App :: atom(), Reason :: term()}}.

start_all_applications([] = _Apps) ->
  ok;
start_all_applications([App | Rest] = _Apps) ->
  case indira:start_rec(App) of
    ok -> start_all_applications(Rest);
    {error, Reason} -> {error, {App, Reason}}
  end.

%% }}}
%%----------------------------------------------------------
%% validate and set Indira options {{{

%% @doc Set Indira application's environment parameters.
%%
%% Function ensures that all required options (`listen' and `command') are
%% present.

-spec set_indira_options([daemon_option()]) ->
    {ok, StartBefore :: [atom()], StartAfter :: [atom()]}
  | {error, Reason}
  when Reason :: invalid_listen_spec | missing_listen_spec
               | invalid_command_handler | missing_command_handler
               | invalid_pidfile
               | invalid_net_config | invalid_net_start.

set_indira_options(Options) ->
  application:load(indira), % TODO: handle error
  set_indira_options([listen, command, pidfile, net, net_start], Options).

%% @doc Workhorse for {@link set_indira_options/1}.

-spec set_indira_options([atom()], [daemon_option()]) ->
    {ok, StartBefore :: [atom()], StartAfter :: [atom()]}
  | {error, Reason}
  when Reason :: invalid_listen_spec | missing_listen_spec
               | invalid_command_handler | missing_command_handler
               | invalid_pidfile
               | invalid_net_config | invalid_net_start.

set_indira_options([listen | Rest] = _Aspects, Options) ->
  ListenSpecs = proplists:get_value(listen, Options, []),
  case check_listen_specs(ListenSpecs) of
    ok ->
      application:set_env(indira, listen, ListenSpecs),
      set_indira_options(Rest, Options);
    {error, Reason} ->
      {error, Reason}
  end;
set_indira_options([command | Rest] = _Aspects, Options) ->
  case proplists:get_value(command, Options) of
    {Mod, _Args} = CommandHandler when is_atom(Mod) ->
      application:set_env(indira, command, CommandHandler),
      set_indira_options(Rest, Options);
    undefined ->
      {error, missing_command_handler};
    _ ->
      {error, invalid_command_handler}
  end;
set_indira_options([pidfile | Rest] = _Aspects, Options) ->
  case proplists:get_value(pidfile, Options) of
    PidFile when is_list(PidFile) orelse is_binary(PidFile) ->
      application:set_env(indira, pidfile, PidFile),
      set_indira_options(Rest, Options);
    undefined ->
      set_indira_options(Rest, Options);
    _ ->
      {error, invalid_pidfile}
  end;
set_indira_options([net | Rest] = _Aspects, Options) ->
  NodeName = proplists:get_value(node_name, Options),
  NameType = proplists:get_value(name_type, Options),
  Cookie = proplists:get_value(cookie, Options, none),
  case check_net_config(NodeName, NameType, Cookie) of
    ok ->
      application:set_env(indira, net, {NodeName, NameType, Cookie}),
      set_indira_options(Rest, Options);
    skip ->
      set_indira_options(Rest, Options);
    {error, Reason} ->
      {error, Reason}
  end;
set_indira_options([net_start | Rest] = _Aspects, Options) ->
  case proplists:get_value(net_start, Options) of
    NetStart when is_boolean(NetStart) ->
      application:set_env(indira, net_start, NetStart),
      set_indira_options(Rest, Options);
    undefined ->
      set_indira_options(Rest, Options);
    _ ->
      {error, invalid_net_start}
  end;
set_indira_options([] = _Aspects, Options) ->
  StartBefore = proplists:get_all_values(start_before, Options),
  StartAfter  = proplists:get_all_values(start_after,  Options),
  {ok, StartBefore, StartAfter}.

%% @doc Verify correctness of listener specifications.

-spec check_listen_specs(term()) ->
  ok | {error, invalid_listen_spec | missing_listen_spec}.

check_listen_specs([_|_] = Specs) ->
  case lists:all(fun ({M,_}) -> is_atom(M); (_) -> false end, Specs) of
    true -> ok;
    false -> {error, invalid_listen_spec}
  end;
check_listen_specs([] = _Specs) ->
  {error, missing_listen_spec};
check_listen_specs(_Specs) ->
  {error, invalid_listen_spec}.

%% @doc Verify correctness of option values for Erlang networking.

-spec check_net_config(term(), term(), term()) ->
  ok | skip | {error, invalid_net_config}.

check_net_config(undefined = _NodeName, _NameType, _Cookie) ->
  skip;
check_net_config(_NodeName, undefined = _NameType, _Cookie) ->
  skip;
check_net_config(NodeName, NameType, undefined = _Cookie) ->
  check_net_config(NodeName, NameType, none);
check_net_config(NodeName, NameType, Cookie)
when (NameType == shortnames orelse NameType == longnames),
     is_atom(NodeName) ->
  case Cookie of
    %none -> % covered by `is_atom(Cookie)'
    %  ok;
    _ when is_atom(Cookie) ->
      ok;
    {file, CookieFile} when is_list(CookieFile) orelse is_binary(CookieFile) ->
      ok;
    _ ->
      {error, invalid_net_config}
  end;
check_net_config(_NodeName, _NameType, _Cookie) ->
  {error, invalid_net_config}.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
