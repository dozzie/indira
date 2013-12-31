%-----------------------------------------------------------------------------
%
% Indira, Mother of D(a)emons.
% Helper application for running Erlang programs as unix daemons.
%
% Indira provides management protocol through TCP/SSL/UNIX/... sockets, which
% helps in following (somewhat tedious) tasks:
%   * check if the daemon finished its starting procedure
%   * order the daemon to shut down
%   * order the daemon to reload configuration (but how to reload, its up to
%     daemon's author)
%   * retrieve metrics/status info from daemon
%   * other management commands
%
% Note that Indira IS NOT a module for loading data. It may be highly
% inefficient. For management interface it shouldn't matter, though: commands
% are typically a low-bandwidth traffic.
%
% This module is both an API for listeners and an implementation of protocol
% parser + command forwarder.
%
%-----------------------------------------------------------------------------

-module(indira).

-behaviour(gen_server).

-export([command/2]).

% public API for supervision tree
-export([start_link/1]).
-export([start/0]).

% gen_server API
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%-----------------------------------------------------------------------------

-include_lib("kernel/include/inet.hrl").

-record(state, {}).

%-----------------------------------------------------------------------------
% public API
%-----------------------------------------------------------------------------

% send command to Indira process
command(Indira, Line) ->
  gen_server:call(Indira, {command, Line}).

%-----------------------------------------------------------------------------
% public API for supervision tree
%-----------------------------------------------------------------------------

start_link(Parent) ->
  % TODO: don't require registering new process
  gen_server:start_link({local, ?MODULE}, ?MODULE, Parent, []).

% convenience wrapper
start() ->
  application:start(indira, transient).

%-----------------------------------------------------------------------------
% gen_server API
%-----------------------------------------------------------------------------

init(Parent) ->
  % I can't call parent until this function finishes; I'll add a message to
  % process' mailbox for handling later (just calling `spawn_listeners/2')
  self() ! {spawn_listeners, Parent},

  State = #state{},
  {ok, State}.

%% @private spawn_listeners(Parent, State) -> {ok, NewState} {{{
spawn_listeners(Parent, State) ->
  {ok, ListenerSup} = indira_sup:start_listener_pool(Parent),

  SpecList = case application:get_env(indira, listen) of
    {ok, L} -> L;
    undefined -> []
  end,

  [indira_sup:start_listener(ListenerSup, ChildSpec) ||
    {EntryModule, Args} <- SpecList,
    ChildSpec <- [EntryModule:supervision_child_spec(self(), Args)]],

  % TODO: I could use remembering children

  {ok, State}.
% }}}

terminate(_Reason, _State) ->
  ok.

handle_call(Request, _From, State) ->
  case Request of
    {command, Command} ->
      io:fwrite("[indira] got command: ~p~n", [Command]),
      {reply, ok, State};
    stop ->
      {stop, normal, ok, State};
    _Any ->
      io:fwrite("[indira] call: WTF? ~p~n", [_Any]),
      {reply, ok, State}
  end.

handle_cast(_Request, State) ->
  io:fwrite("[indira] cast: WTF? ~p~n", [_Request]),
  {noreply, State}.

handle_info({spawn_listeners, Parent}, State) ->
  % adding listeners supervision tree, as promised in `init/1'
  {ok, NewState} = spawn_listeners(Parent, State),
  {noreply, NewState};
handle_info(_Message, State) ->
  io:fwrite("[indira] message: WTF? ~p~n", [_Message]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  io:fwrite("[indira] code change~n"),
  {ok, State}.

%-----------------------------------------------------------------------------
% vim:ft=erlang:foldmethod=marker
