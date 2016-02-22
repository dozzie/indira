%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example command line tool.
%%%
%%%   This is a small controller script that can only do two things: start and
%%%   sleep forever, and order the running instance to shutdown.
%%%
%```
%#!/usr/bin/escript
%
%-define(ADMIN_SOCKET, "command.sock").
%
%main([])     -> main(["--help"]);
%main(["-h"]) -> main(["--help"]);
%main(["--help"]) ->
%  io:fwrite("Usage: ~s start|stop~n", [escript:script_name()]),
%  ok;
%
%main(["start"]) ->
%  indira:set_option(indira, listen, [{indira_unix, ?ADMIN_SOCKET}]),
%  indira:set_option(indira, command, fun(C) -> handle_command(C) end),
%  indira:start_rec(indira),
%  indira:sleep_forever(),
%  ok;
%
%main(["stop"]) ->
%  Command = [{command, stop}],
%  Timeout = 10 * 1000, % 10 seconds
%  case indira:send_one_command(indira_unix, ?ADMIN_SOCKET, Command, Timeout) of
%    {ok, Reply} ->
%      io:fwrite("reply: ~1024p~n", [Reply]);
%    {error, closed} ->
%      io:fwrite("shutdown~n");
%    {error, enoent} ->
%      io:fwrite("already stopped~n");
%    {error, Reason} ->
%      io:fwrite("read error: ~1024p~n", [Reason])
%  end,
%  ok.
%
% % note that this will be passed as a fun, not as a module handler;
% % see `gen_indira_command' docs
%handle_command([{<<"command">>, <<"stop">>}] = _Command) ->
%  init:stop(),
%  [{result, ok}];
%handle_command(_Command) ->
%  [{result, error}, {message, <<"unknown command">>}].
%'''
%%%
%%% @TODO Add more sophisticated command line parsing.
%%% @TODO Add configuration file.
%%% @end
%%%---------------------------------------------------------------------------

-module(examples_escript).

%%% Module for documentation only.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
