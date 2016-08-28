%%%---------------------------------------------------------------------------
%%% @doc
%%%   Command line parser and executor.
%%%
%%%   Following code shows a quite usable skeleton for handling command line
%%%   operations (<i>start</i>/<i>stop</i>/<i>get-pid</i>). When combined with
%%%   {@link examples_escript} and {@link examples_command_handler}, it forms
%%%   a convenient structure for administrative interface.
%%%
%```
%-module(example_cli_handler).
%
%-behaviour(gen_indira_cli).
%
%-export([parse_arguments/2]).
%-export([handle_command/2, format_request/2, handle_reply/3]).
%
%-export([help/1]).
%
%-define(ADMIN_SOCKET_TYPE, indira_unix).
%
%-record(cmd, {
%  op :: start | stop | get_pid,
%  options :: [{atom(), term()}]
%}).
%
%parse_arguments(Args, [DefAdminSocket, DefPidFile] = _Defaults) ->
%  EmptyCommand = #cmd{
%    options = [
%      {socket, DefAdminSocket},
%      {pidfile, DefPidFile}
%    ]
%  },
%  case indira_cli:folds(fun cli_opt/2, EmptyCommand, Args) of
%    {ok, #cmd{op = undefined}} ->
%      help;
%    {ok, Command = #cmd{op = start}} ->
%      {ok, start, Command};
%    {ok, Command = #cmd{op = Op, options = Options}} ->
%      AdminSocketAddr = proplists:get_value(socket, Options),
%      {send, {?ADMIN_SOCKET_TYPE, AdminSocketAddr}, Op, Command};
%    {error, {help, _Arg}} ->
%      help;
%    {error, {Reason, Arg}} ->
%      {error, {Reason, Arg}}
%  end.
%
%handle_command(start = _Op, Command = #cmd{options = Options}) ->
%  AdminSocketAddr = proplists:get_value(socket, Options),
%  case configure_applications(Command) of
%    {ok, IndiraOptions} ->
%      indira_app:daemonize(example_app, [
%        {listen, [{?ADMIN_SOCKET_TYPE, AdminSocketAddr}]},
%        {command, {example_command_handler, []}} |
%        IndiraOptions
%      ]);
%    {error, Reason} ->
%      {error, {configure, Reason}}
%  end.
%
%format_request(Op, _Command) ->
%  Request = example_command_handler:format_request(Op),
%  {ok, Request}.
%
%handle_reply(Reply, get_pid = _Op, _Command) ->
%  case example_command_handler:decode_reply(Reply) of
%    {ok, Pid} -> io:fwrite("~s~n", [Pid]), ok;
%    {error, Message} -> {error, Message}
%  end;
%handle_reply(Reply, stop = _Op, _Command) ->
%  % replies to `stop' are compatible with what is expected here
%  example_command_handler:decode_reply(Reply).
%
%help(Script) ->
%  _Usage = [
%    "Usage:\n",
%    "  ", Script, " start   [--socket PATH] [--pidfile PATH]\n",
%    "  ", Script, " stop    [--socket PATH]\n",
%    "  ", Script, " get-pid [--socket PATH]\n"
%  ].
%
%cli_opt("-h",     _Command) -> {error, help};
%cli_opt("--help", _Command) -> {error, help};
%
%cli_opt("--socket", _Command) ->
%  {need, 1};
%cli_opt(["--socket", Path], Command = #cmd{options = Options}) ->
%  _NewCommand = Command#cmd{options = [{socket, Path} | Options]};
%
%cli_opt("--pidfile", _Command) ->
%  {need, 1};
%cli_opt(["--pidfile", Path], Command = #cmd{options = Options}) ->
%  _NewCommand = Command#cmd{options = [{pidfile, Path} | Options]};
%
%cli_opt("-" ++ _, _Command) ->
%  {error, unknown_option};
%
%cli_opt(Arg, Command = #cmd{op = undefined}) ->
%  case Arg of
%    "start"   -> _NewCommand = Command#cmd{op = start};
%    "stop"    -> _NewCommand = Command#cmd{op = stop};
%    "get-pid" -> _NewCommand = Command#cmd{op = get_pid};
%    _ -> {error, unknown_command}
%  end;
%
%cli_opt(_Arg, _Command) ->
%  % not an option and the operation was already defined
%  {error, excesive_argument}.
%
%configure_applications(Command = #cmd{options = Options}) ->
%  case configure_example_app(Command) of
%    ok ->
%      PidFile = proplists:get_value(pidfile, Options),
%      IndiraOptions = [
%        {pidfile, PidFile}
%        % maybe other options, like `workdir' or `start_after'
%      ],
%      {ok, IndiraOptions};
%    {error, Reason} ->
%      {error, Reason}
%  end.
%
%configure_example_app(_Command) ->
%  % this obviously is a stub; this would be an excellent place to load
%  % INI/TOML/YAML file specified in command line and call
%  % `indira_app:set_env/4' on data loaded from the config file
%  ok.
%'''
%%% @end
%%%---------------------------------------------------------------------------

-module(examples_cli_handler).

%%% Module for documentation only.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
