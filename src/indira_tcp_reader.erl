%%%---------------------------------------------------------------------------
%%%
%%% TCP connection handler.
%%%
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

-record(client, {socket, command_recipient}).

%%%---------------------------------------------------------------------------
%%% public API for supervision tree
%%%---------------------------------------------------------------------------

%% spawn process that reads everything from TCP socket
start_link(CmdRecipient, ClientSocket) ->
  gen_server:start_link(?MODULE, {CmdRecipient, ClientSocket}, []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

init({CmdRecipient, ClientSocket}) ->
  State = #client{socket = ClientSocket, command_recipient = CmdRecipient},
  {ok, State}.


terminate(_Reason, _State = #client{socket = Socket}) ->
  gen_tcp:close(Socket),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {noreply, State}. % ignore unknown calls


handle_cast(_Request, State) ->
  {noreply, State}. % ignore unknown calls


handle_info({tcp_closed, Socket}, State = #client{socket = Socket}) ->
  gen_tcp:close(Socket),
  {stop, normal, State};

handle_info({tcp, Socket, Line}, State = #client{socket = Socket}) ->
  indira:command(State#client.command_recipient, Line),
  {noreply, State};

handle_info(_Any, State = #client{}) ->
  {noreply, State}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
