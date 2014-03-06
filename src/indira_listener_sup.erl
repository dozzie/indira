%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Supervisor for listeners.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_listener_sup).

-behaviour(supervisor).

%% public API
-export([start_link/0]).
-export([start_listener/2]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%% @doc Start the supervisor process.
start_link() ->
  supervisor:start_link(?MODULE, []).

%%%---------------------------------------------------------------------------

%% @doc Start new listener child under the supervisor.
start_listener(Supervisor, {{M,F,A}, ChildType} = _Spec) ->
  ChildSpec = {
    % I don't plan to manually stop/restart children anyway, and this function
    % is supposed to be called in a single, short burst on supervisor with no
    % children (just after spawning it), so non-uniqueness of refs is not
    % a problem here
    make_ref(),
    {M, F, A},
    permanent, 5000, ChildType, [M]
  },
  % strip `Info' field from `{ok, Child, Info}' tuple, to always return
  % `{ok, Child}' on success
  case supervisor:start_child(Supervisor, ChildSpec) of
    {ok, _Child} = Result ->
      Result;
    {ok, Child, _Info} ->
      {ok, Child};
    Error ->
      Error
  end.

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize supervisor.
init([] = _Args) ->
  Strategy = {one_for_one, 5, 10},
  Children = [],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
