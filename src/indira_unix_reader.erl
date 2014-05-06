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

-record(state, {socket, command_router}).

%%%---------------------------------------------------------------------------
%%% public API for supervision tree
%%%---------------------------------------------------------------------------

%% @doc Start TCP reader process.
start_link({CommandRouter, ClientSocket} = _Args) ->
  gen_server:start_link(?MODULE, {CommandRouter, ClientSocket}, []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize {@link gen_server} state.
init({CommandRouter, ClientSocket} = _Args) ->
  State = #state{socket = ClientSocket, command_router = CommandRouter},
  {ok, State}.

%% @doc Clean up {@link gen_server} state.
terminate(_Reason, _State = #state{socket = Socket}) ->
  indira_af_unix_connection:close(Socket),
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
handle_info({unix_closed, Socket} = _Msg, State = #state{socket = Socket}) ->
  {stop, normal, State};

handle_info({unix, Socket, Line} = _Msg, State = #state{socket = Socket}) ->
  % log-and-terminate on parse error
  case indira:command(State#state.command_router, Line) of
    ok ->
      {noreply, State};
    {error, Reason} ->
      Client = {unix, unknown_peer},
      indira:log_error(bad_command_line, Reason,
                       [{command_line, Line}, {client, Client}]),
      {stop, bad_command_line, State}
  end;

handle_info({result, Line} = _Msg, State = #state{socket = Socket}) ->
  indira_af_unix_connection:send(Socket, [Line, "\n"]),
  {noreply, State};

handle_info(_Msg, State = #state{}) ->
  {noreply, State}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
