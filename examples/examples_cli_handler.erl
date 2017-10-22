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
%-record(opts, {
%  op :: start | stop | get_pid,
%  options :: [{atom(), term()}]
%}).
%
%parse_arguments(Args, [DefAdminSocket, DefPidFile, DefConfig] = _Defaults) ->
%  EmptyCommand = #opts{
%    options = [
%      {config, DefConfig},
%      {socket, DefAdminSocket},
%      {pidfile, DefPidFile}
%    ]
%  },
%  case gen_indira_cli:folds(fun cli_opt/2, EmptyCommand, Args) of
%    {ok, #opts{op = undefined}} ->
%      help;
%    {ok, Options = #opts{op = start}} ->
%      {ok, start, Options};
%    {ok, Options = #opts{op = Op, options = CLIOpts}} ->
%      AdminSocketAddr = proplists:get_value(socket, CLIOpts),
%      {send, {?ADMIN_SOCKET_TYPE, AdminSocketAddr}, Op, Options};
%    {error, {help, _Arg}} ->
%      help;
%    {error, {_Reason, _Arg} = Error} ->
%      {error, {help, Error}}
%  end.
%
%handle_command(start = _Op, _Options = #opts{options = CLIOpts}) ->
%  AdminSocketAddr = proplists:get_value(socket, CLIOpts),
%  ConfigFile = proplists:get_value(config, CLIOpts),
%  PidFile = proplists:get_value(pidfile, CLIOpts),
%  case load_config(ConfigFile) of
%    {ok, AppEnv, IndiraOptions} ->
%      ok = indira:set_env(example_app, AppEnv),
%      indira:daemonize(example_app, [
%        {listen, [{?ADMIN_SOCKET_TYPE, AdminSocketAddr}]},
%        {command, {example_command_handler, []}},
%        {pidfile, PidFile} |
%        IndiraOptions
%      ]);
%    {error, Reason} ->
%      {error, {load_config, Reason}}
%  end.
%
%format_request(Op, _Options = #opts{}) ->
%  Request = example_command_handler:format_request(Op),
%  {ok, Request}.
%
%handle_reply(Reply, get_pid = _Op, _Options = #opts{}) ->
%  case example_command_handler:decode_reply(Reply) of
%    {ok, Pid} -> io:fwrite("~s~n", [Pid]), ok;
%    {error, Message} -> {error, Message}
%  end;
%handle_reply(Reply, stop = _Op, _Options = #opts{}) ->
%  % replies to `stop' are compatible with what is expected here
%  example_command_handler:decode_reply(Reply).
%
%help(ScriptName) ->
%  _Usage = [
%    "Usage:\n",
%    "  ", ScriptName, " start   [--socket PATH] [--pidfile PATH]\n",
%    "  ", ScriptName, " stop    [--socket PATH]\n",
%    "  ", ScriptName, " get-pid [--socket PATH]\n",
%    ""
%  ].
%
%cli_opt("-h",     _Opts) -> {error, help};
%cli_opt("--help", _Opts) -> {error, help};
%
%cli_opt("--socket", _Opts) ->
%  {need, 1};
%cli_opt(["--socket", Path], Opts = #opts{options = Options}) ->
%  _NewOpts = Opts#opts{options = [{socket, Path} | Options]};
%
%cli_opt("--pidfile", _Opts) ->
%  {need, 1};
%cli_opt(["--pidfile", Path], Opts = #opts{options = Options}) ->
%  _NewOpts = Opts#opts{options = [{pidfile, Path} | Options]};
%
%cli_opt("-" ++ _, _Opts) ->
%  {error, unknown_option};
%
%cli_opt(Arg, Opts = #opts{op = undefined}) ->
%  case Arg of
%    "start"   -> _NewOpts = Opts#opts{op = start};
%    "stop"    -> _NewOpts = Opts#opts{op = stop};
%    "get-pid" -> _NewOpts = Opts#opts{op = get_pid};
%    _ -> {error, unknown_command}
%  end;
%
%cli_opt(_Arg, _Opts) ->
%  % not an option and the operation was already defined
%  {error, excesive_argument}.
%
%load_config(ConfigFile) ->
%  case file:consult(ConfigFile) of
%    {ok, Sections} ->
%      AppDefault = indira:default_env(example_app),
%      AppEnv = proplists:get_value(example_app, Sections, []),
%      IndiraOptions = proplists:get_value(indira, Sections, []),
%      {ok, AppEnv ++ AppDefault, IndiraOptions};
%    {error, Reason} ->
%      {error, Reason}
%  end.
%'''
%%%
%%% Config file loaded by `load_config()' function above could look like this:
%%%
%```
%{example_app, [
%  % sequence of pairs {Key :: atom(), Value :: term()}, whatever the example
%  % application `example_app' needs
%]}.
%
%{indira, [
%  {node_name, example_app},
%  {name_type, shortnames},
%  %{cookie, {file, "/etc/example_app/cookie.txt"}},
%  {net_start, false}
%]}.
%'''
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(examples_cli_handler).

%%% Module for documentation only.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
