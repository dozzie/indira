%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   UNIX connection handler.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_unix_reader).

-behaviour(gen_server).

%% public API for supervision tree
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {socket}).

%%%---------------------------------------------------------------------------
%%% public API for supervision tree
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start unix reader process.

start_link(ClientSocket) ->
  gen_server:start_link(?MODULE, [ClientSocket], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization and termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init([ClientSocket] = _Args) ->
  State = #state{socket = ClientSocket},
  {ok, State}.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Reason, _State = #state{socket = Socket}) ->
  indira_af_unix:close(Socket),
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

handle_call(stop = _Request, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {noreply, State}. % ignore unknown calls

%% @private
%% @doc Handle {@link gen_server:cast/2}.

handle_cast(_Request, State) ->
  {noreply, State}. % ignore unknown calls

%% @private
%% @doc Handle incoming messages (unix data and commands).

handle_info({unix_closed, Socket} = _Msg, State = #state{socket = Socket}) ->
  {stop, normal, State};

handle_info({unix, Socket, Line} = _Msg, State = #state{socket = Socket}) ->
  % log-and-terminate on parse error
  gen_indira_listener:command(Line),
  {noreply, State};

handle_info({result, Line} = _Msg, State = #state{socket = Socket}) ->
  indira_af_unix:send(Socket, [Line, "\n"]),
  {noreply, State};

handle_info(_Msg, State = #state{}) ->
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
%%% vim:ft=erlang:foldmethod=marker
