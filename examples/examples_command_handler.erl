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
%-export([prepare_status/0, prepare_status/1, result_status/1]).
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
%handle_command([{<<"command">>, <<"status">>}, {<<"wait">>, false}] = _Command,
%               _Arg) ->
%  Running = check_running(),
%  [{running, Running}];
%handle_command([{<<"command">>, <<"status">>}, {<<"wait">>, true}] = _Command,
%               _Arg) ->
%  wait_for_running(),
%  [{running, true}]; % because we waited for it
%
%handle_command(_Command, _Arg) ->
%  [{error, <<"unrecognized command">>}].
%
% %%--------------------------------------------------
%
%check_running() ->
%  % NOTE: you want stronger check, like looking at what my_application_sup's
%  % children are started, as this is true as soon as the booting started, not
%  % as it finished
%  case whereis(my_application_sup) of
%    Pid when is_pid(Pid) -> true;
%    undefined -> false
%  end.
%
%wait_for_running() ->
%  case check_running() of
%    true -> ok;
%    false -> timer:sleep(100), wait_for_running()
%  end.
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
%prepare_status() ->
%  prepare_status(false).
%
%prepare_status(Wait) ->
%  [{<<"command">>, <<"status">>}, {<<"wait">>, Wait}].
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
%result_status([{running, true}] = _Result) ->
%  running;
%result_status([{running, false}] = _Result) ->
%  not_running.
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
