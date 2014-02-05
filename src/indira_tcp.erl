%%%---------------------------------------------------------------------------
%%% @doc
%%%   TCP listener entry point.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_tcp).

-behaviour(gen_indira_listener).

%% Indira listener API
-export([supervision_child_spec/2]).

%%%---------------------------------------------------------------------------
%%% Indira listener API
%%%---------------------------------------------------------------------------

%% @doc Listener description.
supervision_child_spec(CmdRouter, {Host, Port} = _Args) ->
  MFA = {indira_tcp_sup, start_link, [CmdRouter, Host, Port]},
  {MFA, supervisor}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
