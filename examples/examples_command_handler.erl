%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example command handler.
%%%
%%%   Command handler may be either a callback module ({@section Command
%%%   handler module}) or a function ({@section Command handler function}).
%%%
%%%   The module-based approach is recommended, as function-based one
%%%   interferes with code upgrade mechanism.
%%%
%%%   == Command handler module ==
%%%
%%%   === Minimal command handler ===
%%%
%%%   This is the smallest useful command handler module. For real use it's
%%%   not too convenient, though, as you need to store the structure of
%%%   commands and replies in two places: in this handler and in the script
%%%   that talks to Indira.
%%%
%```
%-module(example_command_handler).
%
%-behaviour(gen_indira_command).
%
%-export([handle_command/2]).
%
%handle_command([{<<"command">>, <<"stop">>}] = _Command, _Arg) ->
%  % in the client, be prepared to have connection closed before any message
%  % comes through
%  init:stop(),
%  [{result, ok}];
%
% % other commands...
%
%handle_command(Command, _Arg) ->
%  io:fwrite("got command ~p~n", [Command]),
%  [{error, <<"unrecognized command">>}].
%'''
%%%
%%%   === More advanced command handler ===
%%%
%%%   A little more useful in the long run is to have all three things in the
%%%   same place: encoding a command in a JSON-serializable structure,
%%%   reacting to it (`handle_command()'), and decoding a reply.
%%%   Together with appropriate examples from {@link examples_cli_handler} and
%%%   {@link examples_escript}, this module forms a skeleton for a convenient
%%%   administrative interface to an application.
%%%
%```
%-module(example_command_handler).
%
%-behaviour(gen_indira_command).
%
%-export([handle_command/2]).
%-export([format_request/1, decode_reply/1]).
%
%handle_command([{<<"command">>, <<"stop">>}] = _Command, _Arg) ->
%  % in the client, be prepared to have connection closed before any message
%  % comes through
%  init:stop(),
%  [{result, ok}];
%
%handle_command([{<<"command">>, <<"get_pid">>}] = _Command, _Arg) ->
%  OSPid = list_to_binary(os:getpid()),
%  [{result, ok}, {pid, OSPid}];
%
% % other commands...
%
%handle_command(Command, _Arg) ->
%  io:fwrite("got command ~p~n", [Command]),
%  [{error, <<"unrecognized command">>}].
%
%format_request(stop    = _Command) -> [{<<"command">>, <<"stop">>}];
%format_request(get_pid = _Command) -> [{<<"command">>, <<"get_pid">>}];
% % other commands...
%
%decode_reply([{<<"result">>, <<"ok">>}] = _Reply) ->
%  ok;
%decode_reply([{<<"pid">>, Pid}, {<<"result">>, <<"ok">>}] = _Reply) ->
%  % when using `gen_indira_cli' or `indira_cli:send_one_command()', decoded
%  % replies are orddict-compatible, so the keys in `Reply' are sorted
%  {ok, Pid};
%decode_reply([{<<"error">>, Message}] = _Reply) ->
%  {error, Message}.
%'''
%%%
%%%   This way all the knowledge about requests structure is in one place and
%%%   the code for `escript' or {@link gen_indira_cli} uses much simpler
%%%   interface:
%%%
%```
% % escript
%Request = example_command_handler:format_request(get_pid),
%{ok, Reply} = indira_cli:send_one_command(
%  ?ADMIN_SOCK_MODULE, ?ADMIN_SOCK_ADDRESS,
%  Request,
%  []
%),
%{ok, Pid} = example_command_handler:decode_reply(Reply),
%io:fwrite("~s~n", [Pid]).
%
% % gen_indira_cli
%format_request(Op, _Command) when Op == stop; Op == get_pid ->
%  Request = example_command_handler:format_request(Op),
%  {ok, Request}.
%
%handle_reply(Reply, get_pid = _Op, _Command) ->
%  case example_command_handler:decode_reply(Reply) of
%    {ok, Pid} -> io:fwrite("~s~n", [Pid]), ok;
%    {error, Message} -> {error, Message}
%  end;
%handle_reply(Reply, stop = _Op, _Command) ->
%  example_command_handler:decode_reply(Reply).
%'''
%%%
%%%   == Command handler function ==
%%%
%%%   For times when you <em>really</em> want to avoid adding a separate
%%%   module, you can still provide a function that will handle the commands.
%%%
%```
%#!/usr/bin/escript
%
%main(_Args) ->
%  application:load(indira),
%  application:set_env(indira, listen, [...]),
%  application:set_env(indira, command, fun(C) -> handle_command_fun(C) end),
%  indira_app:start_rec(indira),
%  indira_app:sleep_forever().
%
%handle_command_fun(<<"stop">> = _Command) ->
%  init:stop(),
%  [{result, ok}];
%handle_command_fun(Command) ->
%  io:fwrite("got command ~p~n", [Command]),
%  [{error, <<"unrecognized command">>}].
%'''
%%%
%%%   This way of providing command handlers is not recommended, as it's
%%%   difficult to reload handler's code. Using module is much better in this
%%%   regard.
%%% @end
%%%---------------------------------------------------------------------------

-module(examples_command_handler).

%%% Module for documentation only.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
