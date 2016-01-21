%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Command handler process.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_command).

-behaviour(gen_server).

%% supervision tree API
-export([start/4, start_link/4]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {
  module,
  arg,
  line,
  replyto
}).

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start command handler process.

start(Module, Arg, Command, ReplyTo) ->
  gen_server:start(?MODULE, [Module, Arg, Command, ReplyTo], []).

%% @private
%% @doc Start command handler process.

start_link(Module, Arg, Command, ReplyTo) ->
  gen_server:start_link(?MODULE, [Module, Arg, Command, ReplyTo], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize event handler.

init([Module, Arg, Line, ReplyTo] = _Args) ->
  State = #state{
    module = Module,
    arg = Arg,
    line = Line,
    replyto = ReplyTo
  },
  {ok, State, 0}.

%% @private
%% @doc Clean up after event handler.

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

%% unknown casts
handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
%% @doc Handle incoming messages.

%% do all the job intended for this process
handle_info(timeout = _Message, State = #state{}) ->
  case decode_line(State) of
    {ok, Command} ->
      Response = handle_command(Command, State),
      case encode_response(Response) of
        {ok, Line} ->
          send_response(Line, State);
        {error, _Reason} ->
          indira_log:error(command_error,
                           [{type, bad_response}, {response, Response}])
      end;
    {error, _Reason} ->
      indira_log:error(command_error,
                       [{type, bad_request}, {request, State#state.line}])
  end,
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
%%% helpers
%%%---------------------------------------------------------------------------

%% @doc Decode line sent by client.

-spec decode_line(#state{}) ->
  {ok, gen_indira_command:command()} | {error, term()}.

decode_line(_State = #state{line = Line}) ->
  indira_json:decode(Line).

%% @doc Encode response to send to client.

-spec encode_response(indira_json:struct()) ->
  {ok, iolist()} | {error, term()}.

encode_response(Response) ->
  indira_json:encode(Response).

%% @doc Call appropriate module and return its returned value.

-spec handle_command(gen_indira_command:command(), #state{}) ->
  gen_indira_command:command_response().

handle_command(Command, _State = #state{module = Module, arg = Arg}) ->
  try
    Module:handle_command(Command, Arg)
  catch
    error:Reason ->
      indira_log:error(command_error, [{type, error}, {reason, Reason}]),
      [{exception, <<"unexpected error">>}];
    exit:Reason ->
      indira_log:error(command_error, [{type, exit}, {reason, Reason}]),
      [{exception, <<"unexpected exit">>}];
    throw:Value ->
      indira_log:error(command_error, [{type, throw}, {value, Value}]),
      [{exception, <<"uncaught throw">>}]
  end.

%% @doc Send a response to a client.

-spec send_response(iolist(), #state{}) ->
  ok.

send_response(Response, _State = #state{replyto = Pid}) when is_pid(Pid) ->
  Pid ! {result, Response},
  ok;
send_response(Response, _State = #state{replyto = {Pid, Hint}}) ->
  Pid ! {result, Hint, Response},
  ok.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
