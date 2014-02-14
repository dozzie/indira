%%%---------------------------------------------------------------------------
%%% @doc
%%%   {@link error_logger} log handler (print to <i>STDOUT</i>).
%%%
%%% @TODO Implement this handler.
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_log_stdout_h).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).

-record(state, {}).

%%%---------------------------------------------------------------------------
%%% gen_event callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize event handler.
init(_Args) ->
  {ok, #state{}}.

%% @doc Clean up after event handler.
terminate(_Arg, _State) ->
  ok.

%% @doc Handle incoming events.
handle_event(_Event, State) ->
  {ok, State}.

%% @doc Handle {@link gen_event:call/3}.
handle_call(_Request, State) ->
  {ok, ok, State}.

%% @doc Handle incoming messages.
handle_info(_Message, State) ->
  {ok, State}.

%% @doc Handle code change.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
