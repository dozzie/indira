%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira listener (TCP, SSL, UNIX, you name it) descriptor behaviour.
%%%
%%%   Module implementing this behaviour is an entry point for Indira to spawn
%%%   a listener, either directly a worker (e.g. for connection-less
%%%   protocols) or a supervision subtree (e.g. for TCP).
%%%
%%%   == Module API ==
%%%
%%%   `Module:supervision_child_spec/2' gets two arguments: Indira handle
%%%   (suitable for {@link indira:command/2}) and term that specified as
%%%   module argument in environment specification.
%%%
%%%   `Module:supervision_child_spec/2' is supposed to return `{MFA, Type}',
%%%   where `Type' is `worker' or `supervisor' and `MFA' is `{Module,
%%%   Function, Args}' suitable for {@link erlang:apply/3}. Semantics the same
%%%   as `StartFunc' in child specification in supervisor.
%%%
%%%   Typically, for module `foo' it would be:
%%%   <ul>
%%%     <li>`{{foo, start_link, []}, worker}' for `foo:start_link/0' that runs
%%%         worker</li>
%%%     <li>`{{foo_sup, start_link, []}, supervisor}' for
%%%         `foo_sup:start_link/0' that runs whole supervision tree</li>
%%%   </ul>
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_listener).

-export([behaviour_info/1]).

%%%---------------------------------------------------------------------------

%% @doc Behaviour description.
behaviour_info(callbacks = _Aspect) ->
  [{supervision_child_spec, 2}];
behaviour_info(_Aspect) ->
  undefined.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
