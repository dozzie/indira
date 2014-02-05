%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira message (command and reply) router.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_router).

-behaviour(gen_server).

%% supervision tree API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {}).

%%%---------------------------------------------------------------------------
%%% API for supervision tree
%%%---------------------------------------------------------------------------

%% @doc Start router process.
start_link(Parent) ->
  % TODO: don't require registering new process
  gen_server:start_link({local, ?MODULE}, ?MODULE, Parent, []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize {@link gen_server} state.
init(Parent) ->
  % I can't call parent until this function finishes; I'll add a message to
  % process' mailbox for handling later (just calling `spawn_listeners/2')
  self() ! {spawn_listeners, Parent},

  State = #state{},
  {ok, State}.

%% spawn_listeners(Parent, State) -> {ok, NewState} {{{
%% @doc Spawn listeners defined for this Indira instance.
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

%% @doc Clean up {@link gen_server} state.
terminate(_Reason, _State) ->
  ok.

%% @doc Handle {@link gen_server:call/2}.
handle_call(Request, _From, State) ->
  case Request of
    {command, Command} ->
      % TODO: error_logger:info_report()
      io:fwrite("[indira router] got command: ~p~n", [Command]),
      {reply, ok, State};
    stop ->
      {stop, normal, ok, State};
    _Any ->
      % TODO: error_logger:info_report()
      io:fwrite("[indira router] call: WTF? ~p~n", [_Any]),
      {reply, ok, State}
  end.

%% @doc Handle {@link gen_server:cast/2}.
handle_cast(_Request, State) ->
  % TODO: error_logger:info_report()
  io:fwrite("[indira router] cast: WTF? ~p~n", [_Request]),
  {noreply, State}.

%% @doc Handle incoming messages.
handle_info({spawn_listeners, Parent}, State) ->
  % adding listeners supervision tree, as promised in `init/1'
  {ok, NewState} = spawn_listeners(Parent, State),
  {noreply, NewState};
handle_info(_Message, State) ->
  % TODO: error_logger:info_report()
  io:fwrite("[indira router] message: WTF? ~p~n", [_Message]),
  {noreply, State}.

%% @doc Handle code change.
code_change(_OldVsn, State, _Extra) ->
  % TODO: error_logger:info_report()
  io:fwrite("[indira router] code change~n"),
  {ok, State}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
