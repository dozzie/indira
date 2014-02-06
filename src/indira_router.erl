%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira message (command and reply) router.
%%%
%%% @see gen_indira_listener
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_router).

-behaviour(gen_server).

%% supervision tree API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% command routing API
-export([command/2, command/3]).

%%%---------------------------------------------------------------------------

-record(state, {commander}).

%%%---------------------------------------------------------------------------
%%% API for supervision tree
%%%---------------------------------------------------------------------------

%% @doc Start router process.
start_link(Parent) ->
  case application:get_env(indira, commander) of
    {ok, Cmder} ->
      % TODO: don't require registering new process
      gen_server:start_link({local, ?MODULE}, ?MODULE, {Parent, Cmder}, []);
    undefined ->
      % TODO: or allow start and then reconfiguration?
      {error, no_commander}
  end.

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize {@link gen_server} state.
init({Parent, Commander} = _Args) ->
  % I can't call parent until this function finishes; I'll add a message to
  % process' mailbox for handling later (just calling `spawn_listeners/2')
  self() ! {spawn_listeners, Parent},

  State = #state{commander = Commander},
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
handle_call({command, ReplyTo, Line} = _Request, _From, State) ->
  % I wanted to make this a pure message, but it turned out that I want parse
  % error reporting
  Result = case parse_line(Line) of
    {ok, Command} ->
      io:fwrite("[indira router] got command: ~200p -> ~200p -> ~200p~n",
                [ReplyTo, Command, State#state.commander]),
      State#state.commander ! {command, self(), ReplyTo, Command},
      ok;
    {error, _Reason} = Error ->
      % TODO: error_logger:warning_report()
      Error
  end,
  % let the caller crash on non-ok
  {reply, Result, State};

handle_call(stop = _Request, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
  % TODO: error_logger:info_report()
  io:fwrite("[indira router] call: WTF? ~p~n", [_Request]),
  {reply, ok, State}.

%% @doc Handle {@link gen_server:cast/2}.
handle_cast(_Request, State) ->
  % TODO: error_logger:info_report()
  io:fwrite("[indira router] cast: WTF? ~p~n", [_Request]),
  {noreply, State}.

%% @doc Handle incoming messages.
handle_info({spawn_listeners, Parent} = _Message, State) ->
  % adding listeners supervision tree, as promised in `init/1'
  {ok, NewState} = spawn_listeners(Parent, State),
  {noreply, NewState};

handle_info({result, ReplyTo, Reply} = _Message, State) ->
  % send result back to listener
  io:fwrite("[indira router] got command result: ~200p -> ~200p~n",
            [Reply, ReplyTo]),

  case indira_proto_serializer:encode(Reply) of
    {ok, JSON} ->
      case ReplyTo of
        {Pid, Hint} -> Pid ! {result, Hint, JSON};
        _Pid -> ReplyTo ! {result, JSON}
      end;
    {error, _Reason} ->
      % command executor sent an invalid structure
      % TODO: error_logger:error_report()
      ignore
  end,
  {noreply, State};

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
%%% command routing API
%%%---------------------------------------------------------------------------

%% @doc Send command to Indira router.
%%   Response to the command will be passed as a message to the caller of this
%%   function.
%%
%%   Function may fail on syntax error, `{error,Reason}' will be returned in
%%   such case.
%%
%% @spec command(pid(), binary() | string()) ->
%%   ok | {error, Reason}
command(Indira, Line) ->
  gen_server:call(Indira, {command, self(), Line}).

%% @doc Send command to Indira router.
%%   Response to the command will be passed as a message to the caller of this
%%   function.
%%
%%   Function may fail on syntax error, `{error,Reason}' will be returned in
%%   such case.
%%
%%   `RoutingKey' is an additional information to tell apart between multiple
%%   clients and will be included in command reply message.
%%
%%   This call form is only needed when a single process handles multiple
%%   clients.
%%
%% @spec command(pid(), term(), binary() | string()) ->
%%   ok | {error, Reason}
command(Indira, RoutingKey, Line) ->
  gen_server:call(Indira, {command, {self(), RoutingKey}, Line}).

%%%---------------------------------------------------------------------------
%%% internal helpers
%%%---------------------------------------------------------------------------

%% @doc Parse command line to Erlang data structure.
parse_line(Line) when is_binary(Line) ->
  parse_line(binary_to_list(Line));
parse_line(Line) ->
  case indira_proto_lexer:string(Line) of
    {ok, Tokens, _EndLine} ->
      % TODO: what to do with non-empty _EndLine?
      case indira_proto_parser:parse(Tokens) of
        {ok, Result} ->
          {ok, Result};
        {error, {_LineNumber, _ParserModule, _Message}} ->
          {error, badarg}
      end;
    {error, {_LineNumber, _LexerModule, _Message}, _WhatsThis} ->
      {error, badarg}
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
