%-----------------------------------------------------------------------------

-module(indira_sup).

-behaviour(supervisor).

%-----------------------------------------------------------
% public API

-export([start_link/0]).

%-----------------------------------------------------------
% supervisor callbacks

-export([init/1]).

%-----------------------------------------------------------

%-----------------------------------------------------------------------------
% public API
%-----------------------------------------------------------------------------

start_link() ->
  io:fwrite("indira sup: starting~n"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%-----------------------------------------------------------------------------
% supervisor callbacks
%-----------------------------------------------------------------------------

init([]) ->
  io:fwrite("indira sup: init()~n"),
  Sockets = case application:get_env(indira, listen) of
    {ok, Val} -> Val;
    undefined -> []
  end,
  io:fwrite("indira sup: sockets = ~p~n", [Sockets]),
  Strategy = {one_for_one, 5, 10},
  Children = [
    {indira, {indira, start_link, [Sockets]},
             permanent, 5000, worker, [indira]}
  ],
  {ok, {Strategy, Children}}.

%-----------------------------------------------------------------------------
% vim:ft=erlang:foldmethod=marker
