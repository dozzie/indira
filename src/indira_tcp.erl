%%%---------------------------------------------------------------------------
%%%
%%% TCP listener entry point.
%%%
%%%---------------------------------------------------------------------------

-module(indira_tcp).

-behaviour(indira_listener).

%% Indira listener API
-export([supervision_child_spec/2]).

%%%---------------------------------------------------------------------------
%%% Indira listener API
%%%---------------------------------------------------------------------------

supervision_child_spec(CmdRecipient, {Host, Port} = _Args) ->
  MFA = {indira_tcp_sup, start_link, [CmdRecipient, Host, Port]},
  {MFA, supervisor}.

%% could also be:
%%   MFA = {?MODULE, start_link, [args, to, self]},
%%   {MFA, worker}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
