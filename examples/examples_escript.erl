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
%  {ok, Sock} = indira_af_unix:connect(?ADMIN_SOCKET, [{active, false}]),
%  {ok, Cmd} = indira_json:encode([{command, stop}]),
%  ok = indira_af_unix:send(Sock, [Cmd, $\n]),
%  case indira_af_unix:recv(Sock, 0) of
%    {ok, Line} ->
%      {ok, Reply} = indira_json:decode(Line),
%      io:fwrite("reply: ~1024p~n", [Reply]);
%    {error, closed} ->
%      io:fwrite("shutdown~n");
%    {error, Reason} ->
%      io:fwrite("read error: ~1024p~n", [Reason])
%  end,
%  indira_af_unix:close(Sock),
%  ok.
%
%handle_command([{<<"command">>, <<"stop">>}] = _Command) ->
%  init:stop(),
%  [{result, ok}];
%handle_command(_Command) ->
%  [{result, error}, {message, <<"unknown command">>}].
%'''
%%%
%%% @TODO Add more sophisticated command line parsing.
%%% @TODO Add configuration file.
%%% @TODO Use open-send-recv-close helper (to be written).
%%% @end
%%%---------------------------------------------------------------------------

-module(examples_escript).

%%% Module for documentation only.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
