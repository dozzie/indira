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

%% @private
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

  % TODO: report listeners that failed to start for some reason -- or actually
  % move this code to `indira_sup' (`indira_listener_sup') module
  [indira_sup:start_listener(ListenerSup, ChildSpec) ||
    {EntryModule, Args} <- SpecList,
    ChildSpec <- [EntryModule:supervision_child_spec(self(), Args)]],

  % TODO: I could use remembering children

  {ok, State}.
% }}}

%% @private
%% @doc Clean up {@link gen_server} state.
terminate(_Reason, _State) ->
  ok.

%% @private
%% @doc Handle {@link gen_server:call/2}.
handle_call({command, ReplyTo, Line} = _Request, _From, State) ->
  % I wanted to make this a pure message, but it turned out that I want parse
  % error reporting
  Result = case parse_line(Line) of
    {ok, Command} ->
      % Indira is not the place where the commands should be logged, leave it
      % to the commander
      try
        State#state.commander ! {command, self(), ReplyTo, Command}
      catch
        % commander is an atom and nothing is registered there
        error:badarg ->
          indira:log_error(no_commander, [{command, Command}]),
          ignore % TODO: send an error message back to ReplyTo?
      end,
      ok;
    {error, _Reason} = Error ->
      % NOTE: error logging is left to listeners (they can properly format
      % client's address)
      Error
  end,
  % let the caller crash on non-ok
  {reply, Result, State};

handle_call(stop = _Request, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
  % ignore unknown calls
  {reply, ok, State}.

%% @private
%% @doc Handle {@link gen_server:cast/2}.
handle_cast(_Request, State) ->
  % ignore unknown casts
  {noreply, State}.

%% @private
%% @doc Handle incoming messages.
handle_info({spawn_listeners, Parent} = _Message, State) ->
  % adding listeners supervision tree, as promised in `init/1'
  {ok, NewState} = spawn_listeners(Parent, State),
  {noreply, NewState};

handle_info({result, ReplyTo, Reply} = _Message, State) ->
  % send result back to listener
  ReplyLine = case indira_proto_serializer:encode(Reply) of
    {ok, JSON} ->
      JSON;
    {error, Reason} ->
      % command executor sent an invalid structure
      % I can't give more readable client's address than `ReplyTo', but this
      % should happen rarely, anyway
      indira:log_error(bad_command_reply, Reason,
                       [{reply, Reply}, {client_route, ReplyTo}]),
      <<"bad result">>
  end,
  case ReplyTo of
    {Pid, Hint} -> Pid ! {result, Hint, ReplyLine};
    _Pid -> ReplyTo ! {result, ReplyLine}
  end,
  {noreply, State};

handle_info(_Message, State) ->
  % ignore unknown messages
  {noreply, State}.

%% @private
%% @doc Handle code change.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%---------------------------------------------------------------------------
%%% command routing API
%%%---------------------------------------------------------------------------

%% @doc Send command to Indira router.
%%   The process calling this function will get the response as a message.
%%
%%   Function may fail on syntax error, `{error,Reason}' will be returned in
%%   such case. The caller is responsible for logging an error (possibly with
%%   {@link indira:log_error/3}).
%%
%% @spec command(pid(), binary() | string()) ->
%%   ok | {error, Reason}
command(Indira, Line) ->
  gen_server:call(Indira, {command, self(), Line}).

%% @doc Send command to Indira router.
%%   The process calling this function will get the response as a message.
%%
%%   Function may fail on syntax error, `{error,Reason}' will be returned in
%%   such case. The caller is responsible for logging an error (possibly with
%%   {@link indira:log_error/3}).
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
      case indira_proto_parser:parse(Tokens) of
        {ok, Result} ->
          {ok, Result};
        {error, {_LineNumber, _ParserModule, _Message}} ->
          {error, badarg}
      end;
    {error, {_LineNumber, _LexerModule, _Message}, _} ->
      {error, badarg}
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
