%%%---------------------------------------------------------------------------
%%% @doc
%%%   {@link error_logger} log handler (log to file).
%%%
%%% @TODO Implement this handler.
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_log_file_h).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).

-record(state, {}).

%%%---------------------------------------------------------------------------
%%% gen_event callbacks
%%%---------------------------------------------------------------------------

%% @private
%% @doc Initialize event handler.
init(_Args) ->
  {ok, #state{}}.

%% @private
%% @doc Clean up after event handler.
terminate(_Arg, _State) ->
  ok.

%% @private
%% @doc Handle incoming events.
handle_event(_Event, State) ->
  {ok, State}.

%% @private
%% @doc Handle {@link gen_event:call/3}.
handle_call(_Request, State) ->
  {ok, ok, State}.

%% @private
%% @doc Handle incoming messages.
handle_info(_Message, State) ->
  {ok, State}.

%% @private
%% @doc Handle code change.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
