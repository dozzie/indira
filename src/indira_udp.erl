%%%---------------------------------------------------------------------------
%%%
%%% UDP listener.
%%%
%%%---------------------------------------------------------------------------

-module(indira_udp).

-behaviour(indira_listener).
-behaviour(gen_server).

%%%---------------------------------------------------------------------------

%% Indira listener API
-export([supervision_child_spec/2]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% API for supervision tree
-export([start_link/3]).

%%%---------------------------------------------------------------------------

-include_lib("kernel/include/inet.hrl").

-record(state, {socket, command_recipient}).

%%%---------------------------------------------------------------------------
%%% Indira listener API
%%%---------------------------------------------------------------------------

supervision_child_spec(CmdRecipient, {Host, Port} = _Args) ->
  MFA = {?MODULE, start_link, [CmdRecipient, Host, Port]},
  {MFA, worker}.

%%%---------------------------------------------------------------------------
%%% public API for supervision tree
%%%---------------------------------------------------------------------------

%% spawn process that listens on UDP socket
start_link(CmdRecipient, Host, Port) ->
  Args = [CmdRecipient, Host, Port],
  gen_server:start_link(?MODULE, Args, []).

%%%---------------------------------------------------------------------------
%%% connection acceptor
%%%---------------------------------------------------------------------------

init([CmdRecipient, Host, Port]) ->
  % create listening socket
  BindOpt = address_to_bind_option(Host),
  {ok, Socket} = gen_udp:open(Port, BindOpt ++ [
    {active, true}, {reuseaddr, true}, list
  ]),

  State = #state{socket = Socket, command_recipient = CmdRecipient},
  {ok, State}.


terminate(_Reason, _State = #state{socket = Socket}) ->
  gen_udp:close(Socket),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {noreply, State}. % ignore unknown calls


handle_cast(_Request, State) ->
  {noreply, State}. % ignore unknown casts


handle_info({udp, Socket, _IP, _Port, Data}, State = #state{socket = Socket}) ->
  #state{command_recipient = CmdRecipient} = State,
  indira:command(CmdRecipient, "[udp] " ++ Data),
  {noreply, State};

handle_info(_Any, State = #state{}) ->
  {noreply, State}. % ignore other messages


%%%---------------------------------------------------------------------------
%%% network helpers
%%%---------------------------------------------------------------------------

%% resolve DNS address to IP
address_to_bind_option(any) ->
  [];
address_to_bind_option(Addr) when is_list(Addr) ->
  {ok, #hostent{h_addr_list = HostAddrs}} = inet:gethostbyname(Addr),
  [HostAddr | _Rest] = HostAddrs,
  [{ip, HostAddr}];
address_to_bind_option(Addr) when is_tuple(Addr) ->
  [{ip, Addr}].

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
