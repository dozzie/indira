%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Supervisor that watches over command handler processes.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_command_sup).

-behaviour(supervisor).

%% public interface
-export([spawn_command/2]).

%% supervision tree API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Start a new command handler process.

-spec spawn_command(indira_json:json_string(), pid() | {pid(), term()}) ->
  {ok, pid()} | {error, term()}.

spawn_command(Line, ReplyTo) ->
  supervisor:start_child(?MODULE, [Line, ReplyTo]).

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start the supervisor process.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @private
%% @doc Initialize supervisor.

init(_Args) ->
  Strategy = {simple_one_for_one, 5, 10},
  {Module, Arg} = command_callback_module(),
  Children = [
    {undefined,
      {indira_command, start_link, [Module, Arg]},
      temporary, 5000, worker, [indira_command, Module]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% configuration (app environment) helpers
%%%---------------------------------------------------------------------------

%% @doc Determine callback module from application environment.
%%   If the command callback is a function, return Indira's default callback
%%   module {@link indira_command_handle_fun}.

-spec command_callback_module() ->
  {atom(), gen_indira_command:argument()}.

command_callback_module() ->
  case application:get_env(command) of
    {ok, Fun} when is_function(Fun, 1) ->
      {indira_command_handle_fun, {Fun}};
    {ok, {Fun, Arg}} when is_function(Fun, 2) ->
      {indira_command_handle_fun, {Fun, Arg}};
    {ok, {Module, Arg}} when is_atom(Module) ->
      {Module, Arg}
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
