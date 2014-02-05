%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira application entry point.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%%---------------------------------------------------------------------------
%%% application callbacks
%%%---------------------------------------------------------------------------

%% @doc Start application.
start(_StartType, _StartArgs) ->
  indira_sup:start_link().

%% @doc Stop application.
stop(_State) ->
  ok.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
