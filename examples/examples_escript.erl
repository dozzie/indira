%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example command line tool.
%%%
%%%   == Standalone script ==
%%%
%%%   This is a small controller script that has very limited set of commands.
%%%   It is self-contained, however.
%%%
%```
%#!/usr/bin/escript
%
%-define(ADMIN_SOCK_MODULE, indira_unix).
%-define(ADMIN_SOCK_ADDRESS, "control.sock").
%
%main([])     -> main(["--help"]);
%main(["-h"]) -> main(["--help"]);
%main(["--help"]) ->
%  io:fwrite("Usage: ~s start|stop|get-pid~n", [escript:script_name()]);
%
%main(["start"]) ->
%  indira:indira_setup([
%    {listen, [{?ADMIN_SOCK_MODULE, ?ADMIN_SOCK_ADDRESS}]},
%    {command, fun(C) -> handle_command_fun(C) end}
%  ]),
%  indira:start_rec(indira),
%  indira:sleep_forever();
%
%main(["get-pid"]) ->
%  case send_request([{command, get_pid}]) of
%    {ok, [{<<"pid">>, Pid}, {<<"result">>, <<"ok">>}]} ->
%      io:fwrite("~s~n", [Pid]);
%    {error, Reason} ->
%      io:fwrite(standard_error, "error sending a command: ~p~n", [Reason]),
%      halt(1)
%  end;
%
%main(["stop"]) ->
%  case send_request([{command, stop}], timer:seconds(10)) of
%    {ok, [{<<"result">>, <<"ok">>}]} ->
%      ok;
%    {error, closed} -> % XXX: specific to indira_unix
%      io:fwrite("shutdown~n");
%    {error, enoent} -> % XXX: specific to indira_unix
%      io:fwrite("already stopped~n");
%    {error, Reason} ->
%      io:fwrite(standard_error, "error sending a command: ~p", [Reason]),
%      halt(1)
%  end.
%
%send_request(Request) ->
%  gen_indira_cli:send_one_command(
%    ?ADMIN_SOCK_MODULE, ?ADMIN_SOCK_ADDRESS,
%    Request,
%    []
%  ).
%send_request(Request, Timeout) ->
%  gen_indira_cli:send_one_command(
%    ?ADMIN_SOCK_MODULE, ?ADMIN_SOCK_ADDRESS,
%    Request,
%    [{timeout, Timeout}]
%  ).
%
%handle_command_fun([{<<"command">>, <<"stop">>}] = _Command) ->
%  init:stop(),
%  [{result, ok}];
%handle_command_fun([{<<"command">>, <<"get_pid">>}] = _Command) ->
%  OSPid = list_to_binary(os:getpid()),
%  [{result, ok}, {pid, OSPid}];
%handle_command_fun(Command) ->
%  io:fwrite("got command ~p~n", [Command]),
%  [{error, <<"unrecognized command">>}].
%'''
%%%
%%%   == Script to work with gen_indira_cli and gen_indira_command ==
%%%
%%%   This script, together with appropriate examples for {@link
%%%   gen_indira_cli} and {@link gen_indira_command} ({@link
%%%   examples_cli_handler} and {@link examples_command_handler},
%%%   respectively), forms a skeleton for a convenient administrative
%%%   interface to an application.
%%%
%```
%#!/usr/bin/escript
%
%-define(CLI_MODULE, example_cli_handler).
%-define(CONFIG_FILE,  "/etc/example_app/example.conf").
%-define(ADMIN_SOCKET, "/var/run/example_app/control").
%-define(PIDFILE,      "/var/run/example_app/pid").
%
%help() ->
%  help(standard_io).
%help(IO) ->
%  ScriptName = filename:basename(escript:script_name()),
%  io:put_chars(IO, ?CLI_MODULE:help(ScriptName)).
%
%main([])         -> help();
%main(["-h"])     -> help();
%main(["--help"]) -> help();
%
%main(Args) ->
%  % this is a good idea to keep default paths away from the compiled module
%  Defaults = [?ADMIN_SOCKET, ?PIDFILE, ?CONFIG_FILE],
%  case gen_indira_cli:execute(Args, ?CLI_MODULE, Defaults) of
%    ok ->
%      ok;
%    help ->
%      help();
%    {error, {help, Reason}} ->
%      io:fwrite(standard_error, "~s~n~n", [?CLI_MODULE:format_error(Reason)]),
%      help(standard_error),
%      halt(1);
%    {error, Code} when is_integer(Code) ->
%      % error already written to STDERR
%      halt(Code);
%    {error, Reason} ->
%      io:fwrite(standard_error, "~s~n", [?CLI_MODULE:format_error(Reason)]),
%      halt(1)
%  end.
%'''
%%% @end
%%%---------------------------------------------------------------------------

-module(examples_escript).

%%% Module for documentation only.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
