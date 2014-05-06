%%%---------------------------------------------------------------------------
%%% @doc
%%%   UNIX socket listener entry point.
%%%
%%%   == Indira parameter ==
%%%
%%%   This module expects a string `SocketPath' as a parameter (see
%%%   {@link indira}).
%%%
%%%   @TODO Support for mode and ownership.
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_unix).

-behaviour(gen_indira_listener).

%% Indira listener API
-export([supervision_child_spec/2]).

%%%---------------------------------------------------------------------------
%%% Indira listener API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Listener description.

supervision_child_spec(CmdRouter, SocketPath) ->
  SockStreamSupArgs = [
    indira_unix_listener,
    {indira_unix_reader, start_link},
    {CmdRouter, SocketPath}
  ],
  MFA = {indira_sock_stream_sup, start_link, SockStreamSupArgs},
  {MFA, supervisor}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
