%%%---------------------------------------------------------------------------
%%% @doc
%%%   TCP listener entry point.
%%%
%%%   == Indira parameter ==
%%%
%%%   This module expects a tuple `{Host,Port}' as a parameter (see
%%%   {@link indira}). The `Host' part can be:
%%%   <ul>
%%%     <li>`string()'</li>
%%%     <li>`inet:ip_address()' (i.e. `{N1,N2,N3,N4}' for IPv4)</li>
%%%     <li>`` 'any' '' to indicate no binding to any particular interface</li>
%%%   </ul>
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_tcp).

-behaviour(gen_indira_listener).

%% Indira listener API
-export([supervision_child_spec/2]).

%%%---------------------------------------------------------------------------
%%% Indira listener API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Listener description.
supervision_child_spec(CmdRouter, {Host, Port} = _Args) ->
  MFA = {indira_tcp_sup, start_link, [CmdRouter, Host, Port]},
  {MFA, supervisor}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
