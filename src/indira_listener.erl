%%%---------------------------------------------------------------------------
%%%
%%% Indira listener (TCP, SSL, UNIX, you name it) behaviour.
%%%
%%%---------------------------------------------------------------------------

-module(indira_listener).

-export([behaviour_info/1]).

%%%---------------------------------------------------------------------------

%%% `Module:supervision_child_spec/2' gets two arguments: Indira handle
%%% (suitable for `indira:command/2') and term that specified as module
%%% argument in environment specification.
%%%
%%% `Module:supervision_child_spec/2' is supposed to return `{MFA, Type}',
%%% where `Type' is `worker' or `supervisor' and `MFA' is `{Module, Function,
%%% Args}' suitable for `erlang:apply/3'. Semantics the same as `StartFunc' in
%%% child specification in supervisor.
%%%
%%% Typically, for module `foo' it would be:
%%%   * `{{foo, start_link, []}, worker}' for `foo:start_link/0' that runs
%%%     worker
%%%   * `{{foo_sup, start_link, []}, supervisor}' for `foo_sup:start_link/0'
%%%     that runs whole supervision tree

behaviour_info(callbacks) ->
  [{supervision_child_spec, 2}];
behaviour_info(_AnythingElse) ->
  undefined.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
