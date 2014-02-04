%%%---------------------------------------------------------------------------
%%%
%%% Indira application entry point.
%%%
%%%---------------------------------------------------------------------------

-module(indira_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%%---------------------------------------------------------------------------
%%% application callbacks
%%%---------------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  indira_sup:start_link().

stop(_State) ->
  ok.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
