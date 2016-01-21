%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Supervisor for socket listeners.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_listener_sup).

-behaviour(supervisor).

%% public API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
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

init([] = _Args) ->
  Strategy = {one_for_one, 5, 10},
  Children = case application:get_env(indira, listen) of
    undefined       -> []; % TODO: indicate error?
    {ok, Listeners} -> [child_spec(Module, Arg) || {Module, Arg} <- Listeners]
  end,
  {ok, {Strategy, Children}}.

%% @doc Helper to retrieve child specification from {@link
%%   gen_indira_listener} module.

-spec child_spec(module(), term()) ->
  supervisor:child_spec().

child_spec(Module, Arg) ->
  {_Id, {_,_,_} = MFA, _Restart, Shutdown, Type, Modules} =
    Module:child_spec(Arg),
  {make_ref(), MFA, permanent, Shutdown, Type, Modules}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
