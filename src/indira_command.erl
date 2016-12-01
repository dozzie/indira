%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Administrative command handler module. This module executes a command
%%%   handler module (the one that implements {@link gen_indira_command}).
%%%
%%% @see gen_indira_command
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_command).

-behaviour(gen_server).

%% interface for gen_indira_socket
-export([spawn_worker/0]).
-export([execute/2, execute/3]).

%% supervision tree API
-export([start/3, start_link/3]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------
%%% types {{{

-record(state, {
  module :: module(),
  argument :: gen_indira_command:argument(),
  owner :: {pid(), reference()}
}).

%%% }}}
%%%---------------------------------------------------------------------------
%%% interface for gen_indira_socket
%%%---------------------------------------------------------------------------

%% @doc Spawn a new command executor process.

-spec spawn_worker() ->
  {ok, pid()} | {error, term()}.

spawn_worker() ->
  indira_command_sup:spawn_worker(self()).

%% @doc Execute an incoming command line.
%%
%%   Function returns immediately, and the result is later sent as `{result,
%%   ReplyLine}' to the worker's owner (`{error, throw, Reason}' or `{error,
%%   exit | error, Reason, StackTrace}' in case of handler's death or
%%   unserializable result).
%%
%%   Worker process exits as soon as it finishes the job.

-spec execute(pid(), string() | binary()) ->
  ok.

execute(Pid, CommandLine) ->
  gen_server:cast(Pid, {execute, CommandLine}).

%% @doc Execute an incoming command line.
%%
%%   Function returns immediately, and the result is later sent as `{result,
%%   RoutingKey, ReplyLine}' to the worker's owner (`{error, RoutingKey,
%%   throw, Reason}' or `{error, RoutingKey, exit | error, Reason,
%%   StackTrace}' in case of handler's death or unserializable result).
%%
%%   Worker process exits as soon as it finishes the job.

-spec execute(pid(), string() | binary(), term()) ->
  ok.

execute(Pid, CommandLine, RoutingKey) ->
  gen_server:cast(Pid, {execute, CommandLine, RoutingKey}).

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start command executor process.

start(Module, Arg, Owner) ->
  gen_server:start(?MODULE, [Module, Arg, Owner], []).

%% @private
%% @doc Start command executor process.

start_link(Module, Arg, Owner) ->
  gen_server:start_link(?MODULE, [Module, Arg, Owner], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init([Module, Arg, Owner] = _Args) ->
  MonRef = erlang:monitor(process, Owner),
  State = #state{
    module = Module,
    argument = Arg,
    owner = {Owner, MonRef}
  },
  {ok, State}.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Arg, _State) ->
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

%% unknown calls
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

handle_cast({execute, CommandLine} = _Request,
            State = #state{owner = {Owner, _}}) ->
  case execute_line(CommandLine, State) of
    {ok, ReplyLine} ->
      Owner ! {result, ReplyLine};
    {error, {throw, Reason}} ->
      Owner ! {error, throw, Reason};
    {error, {Type, Reason, StackTrace}} ->
      Owner ! {error, Type, Reason, StackTrace};
    {error, Reason} ->
      Owner ! {error, error, Reason, []}
  end,
  {stop, normal, State};

handle_cast({execute, CommandLine, RoutingKey} = _Request,
            State = #state{owner = {Owner, _}}) ->
  case execute_line(CommandLine, State) of
    {ok, ReplyLine} ->
      Owner ! {result, RoutingKey, ReplyLine};
    {error, {throw, Reason}} ->
      Owner ! {error, RoutingKey, throw, Reason};
    {error, {Type, Reason, StackTrace}} ->
      Owner ! {error, RoutingKey, Type, Reason, StackTrace};
    {error, Reason} ->
      Owner ! {error, RoutingKey, error, Reason, []}
  end,
  {stop, normal, State};

%% unknown casts
handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
%% @doc Handle incoming messages.

handle_info({'DOWN', MonRef, process, Pid, _Reason} = _Message,
            State = #state{owner = {Pid, MonRef}}) ->
  {stop, normal, State};

%% unknown messages
handle_info(_Message, State) ->
  {noreply, State}.

%% }}}
%%----------------------------------------------------------
%% code change {{{

%% @private
%% @doc Handle code change.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------

%% @doc Parse incoming line, pass it to `Module' and return a serialized
%%   reply.

-spec execute_line(string() | binary(), #state{}) ->
  {ok, iolist()} | {error, Reason}
  when Reason :: bad_request_format
               | bad_reply_format
               | {throw, term()}
               | {error | exit, term(), StackTrace},
       StackTrace :: [StackEntry],
       StackEntry :: {Mod :: atom(), Fun :: atom(), Arity | Args, Location},
       Arity :: non_neg_integer(),
       Args :: [term()],
       Location :: [{atom(), term()}].

execute_line(CommandLine, _State = #state{module = Module, argument = Arg}) ->
  case indira_json:decode(CommandLine) of
    {ok, Command} ->
      try Module:handle_command(Command, Arg) of
        Reply ->
          case indira_json:encode(Reply) of
            {ok, ReplyLine} -> {ok, ReplyLine};
            {error, badarg} -> {error, bad_reply_format}
          end
      catch
        throw:Reason -> {error, {throw, Reason}};
        error:Reason -> {error, {error, Reason, erlang:get_stacktrace()}};
        exit:Reason  -> {error, {exit,  Reason, erlang:get_stacktrace()}}
      end;
    {error, badarg} ->
      {error, bad_request_format}
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
