%%%---------------------------------------------------------------------------
%%% @doc
%%%   TCP connection handler.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_tcp_reader).

-behaviour(gen_server).

%% public API for supervision tree
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {socket, command_router}).

%%%---------------------------------------------------------------------------
%%% public API for supervision tree
%%%---------------------------------------------------------------------------

%% @doc Start TCP reader process.
start_link(CmdRouter, ClientSocket) ->
  gen_server:start_link(?MODULE, {CmdRouter, ClientSocket}, []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize {@link gen_server} state.
init({CmdRouter, ClientSocket} = _Args) ->
  State = #state{socket = ClientSocket, command_router = CmdRouter},
  {ok, State}.

%% @doc Clean up {@link gen_server} state.
terminate(_Reason, _State = #state{socket = Socket}) ->
  gen_tcp:close(Socket),
  ok.

%% @doc Handle code change.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @doc Handle {@link gen_server:call/2}.
handle_call(stop = _Request, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {noreply, State}. % ignore unknown calls

%% @doc Handle {@link gen_server:cast/2}.
handle_cast(_Request, State) ->
  {noreply, State}. % ignore unknown calls

%% @doc Handle incoming messages (TCP data and commands).
handle_info({tcp_closed, Socket} = _Msg, State = #state{socket = Socket}) ->
  gen_tcp:close(Socket),
  {stop, normal, State};

handle_info({tcp, Socket, Line} = _Msg, State = #state{socket = Socket}) ->
  indira:command(State#state.command_router, Line),
  {noreply, State};

handle_info({result, Line} = _Msg, State = #state{socket = Socket}) ->
  gen_tcp:send(Socket, [Line, "\n"]),
  {noreply, State};

handle_info(_Msg, State = #state{}) ->
  {noreply, State}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
