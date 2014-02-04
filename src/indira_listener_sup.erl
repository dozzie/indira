%%%---------------------------------------------------------------------------
%%%
%%% Supervisor for listeners (see `indira_listener' module).
%%%
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

start_link() ->
  supervisor:start_link(?MODULE, []).

%%%---------------------------------------------------------------------------

%% add new listener child (worker or supervision tree) to existing supervisor
start_listener(Supervisor, {{M,F,A}, ChildType} = _Spec) ->
  Child = {
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
  case supervisor:start_child(Supervisor, Child) of
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

init([]) ->
  Strategy = {one_for_one, 5, 10},
  Children = [],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
