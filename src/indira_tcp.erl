%%%---------------------------------------------------------------------------
%%% @doc
%%%   TCP listener entry point.
%%%   This listener is implemented as {@link gen_indira_sock_stream} module
%%%   (listener and connection handler separated). It should be simple enough
%%%   to serve as an example for your own code, if needed.
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
-export([child_spec/2]).

%%%---------------------------------------------------------------------------
%%% Indira listener API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Listener description.

child_spec(CmdRouter, {_Host,_Port} = BindAddr) ->
  SockStreamSupArgs = [
    indira_tcp_listener,
    {indira_tcp_reader, start_link},
    {CmdRouter, BindAddr}
  ],
  {ignore,
    {indira_sock_stream_sup, start_link, SockStreamSupArgs},
    permanent, 5000, supervisor, [indira_sock_stream_sup]}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
