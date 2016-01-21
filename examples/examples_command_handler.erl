%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example command handler.
%%%
%%%   Command handler may be either a callback module ({@section Command
%%%   handler module}) or a function ({@section Command handler function}).
%%%
%%%   == Command handler module ==
%%%
%%%   Note that only `handle_command/2' here is really required. `prepare_*()'
%%%   and `result_*()' functions are added so all the forming and decoding the
%%%   structures carrying commands and results is in the same place, so they
%%%   can be changed at the same time.
%%%
%```
%-module(command_handler).
%
%-behaviour(gen_indira_command).
%
% %% gen_indira_command behaviour
%-export([handle_command/2]).
%
% %% exports for escript (controlling command)
%-export([prepare_stop/0, result_stop/1]).
%-export([prepare_reload/1, result_reload/1]).
%-export([prepare_check_running/0, result_check_running/1]).
%
% %%--------------------------------------------------
%
%handle_command([{<<"command">>, <<"stop">>}] = _Command, _Arg) ->
%  % in the client, be prepared to have connection closed before any message
%  % comes through
%  init:stop(),
%  [{result, ok}];
%
%handle_command([{<<"command">>, <<"reload">>}, {<<"file">>, File}] = _Command,
%               _Arg) ->
%  case my_application:reload(File) of
%    ok -> [{result, ok}];
%    {error, _Reason} -> [{result, error}] % could include the error message
%  end;
%
%handle_command([{<<"command">>, <<"check-running">>}] = _Command, _Arg) ->
%  case whereis(my_application_sup) of
%    Pid when is_pid(Pid) -> [{result, true}];
%    undefined            -> [{result, false}]
%  end;
%
%handle_command(_Command, _Arg) ->
%  [{error, <<"unrecognized command">>}].
%
% %%--------------------------------------------------
%
%prepare_stop() ->
%  [{command, stop}].
%
%prepare_reload(File) when is_list(File) ->
%  prepare_reload(iolist_to_binary(File));
%prepare_reload(File) when is_binary(File) ->
%  [{command, reload}, {file, File}].
%
%prepare_check_running() ->
%  [{<<"command">>, <<"check-running">>}].
%
% %%--------------------------------------------------
% %% note that these functions die on unexpected reply
%
%result_stop([{result, ok}] = _Result) ->
%  ok;
%result_stop({error, closed} = _Result) ->
%  % Erlang runtime apparently stopped before Indira sent a reply
%  ok.
%
%result_reload([{result, ok}] = _Result) ->
%  ok;
%result_reload([{result, error}] = _Result) ->
%  {error, bad_config}.
%
%result_check_running([{result, true}] = _Result) ->
%  true;
%result_check_running([{result, false}] = _Result) ->
%  false.
%'''
%%%
%%%   This way, controller script (`escript') can use the functions to talk to
%%%   command handler module, similar to this:
%%%
%```
%Command = command_handler:prepare_reload("/etc/foo/foo.conf"),
%Reply = command_handler:result_reload(send_command(Conn, Command)),
%case Reply of
%  ok ->
%    io:fwrite("reload successful~n");
%  {error, Reason} ->
%    io:fwrite("reload failed: ~p~n", [Reason]),
%    halt(1)
%end.
%'''
%%%
%%%   == Command handler function ==
%%%
%%%   This is how to specify a function as a command handler in controlling
%%%   script:
%```
%indira:set_option(indira, command, fun(C) -> handle_command(C) end),
%indira:start_rec(indira).
%'''
%%%
%%%   The `handle_command/1' function is defined as follows:
%%%
%```
%handle_command([{<<"command">>, <<"stop">>}] = _Command) ->
%  init:stop(),
%  [{result, ok}];
%handle_command(_Command) ->
%  [{result, error}, {message, <<"unknown command">>}].
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
