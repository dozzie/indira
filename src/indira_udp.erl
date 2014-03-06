%%%---------------------------------------------------------------------------
%%% @doc
%%%   UDP listener (entry point and actual worker).
%%%
%%%   == Indira parameter ==
%%%
%%%   This module expects a tuple `{Host,Port}' as a parameter (see
%%%   {@link indira}). The `Host' part can be:
%%%   <ul>
%%%     <li>`string()'</li>
%%%     <li>`inet:ip_address()' (i.e. `{N1,N2,N3,N4}' for IPv4)</li>
%%%     <li>`` 'any' '' to indicate no binding to any particular interface</li>
%%%   </ul>
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_udp).

-behaviour(gen_indira_listener).
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

-record(state, {socket, command_router}).

%%%---------------------------------------------------------------------------
%%% Indira listener API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Listener description.
supervision_child_spec(CmdRouter, {Host, Port} = _Args) ->
  MFA = {?MODULE, start_link, [CmdRouter, Host, Port]},
  {MFA, worker}.

%%%---------------------------------------------------------------------------
%%% public API for supervision tree
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start UDP listener process.
start_link(CmdRouter, Host, Port) ->
  Args = {CmdRouter, Host, Port},
  gen_server:start_link(?MODULE, Args, []).

%%%---------------------------------------------------------------------------
%%% connection acceptor
%%%---------------------------------------------------------------------------

%% @private
%% @doc Initialize {@link gen_server} state.
%%   This includes creating listening UDP socket.
init({CmdRouter, Host, Port} = _Args) ->
  % create listening socket
  BindOpt = address_to_bind_option(Host),
  {ok, Socket} = gen_udp:open(Port, BindOpt ++ [
    {active, true}, {reuseaddr, true}, list
  ]),

  State = #state{socket = Socket, command_router = CmdRouter},
  {ok, State}.

%% @private
%% @doc Clean up {@link gen_server} state.
%%   This includes closing the listening socket.
terminate(_Reason, _State = #state{socket = Socket}) ->
  gen_udp:close(Socket),
  ok.

%% @private
%% @doc Handle code change.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @private
%% @doc Handle {@link gen_server:call/2}.
handle_call(stop = _Request, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {noreply, State}. % ignore unknown calls

%% @private
%% @doc Handle {@link gen_server:cast/2}.
handle_cast(_Request, State) ->
  {noreply, State}. % ignore unknown casts

%% @private
%% @doc Handle incoming messages (UDP data and commands).
handle_info({udp, Socket, IP, Port, Line} = _Msg,
            State = #state{socket = Socket}) ->
  #state{command_router = CmdRouter} = State,
  RoutingHint = {IP, Port},
  case indira:command(CmdRouter, RoutingHint, Line) of
    ok ->
      ok;
    {error, Reason} ->
      {ok, Sockname} = inet:sockname(Socket),
      Client = {udp, Sockname, RoutingHint},
      indira:log_error(bad_command_line, Reason,
                       [{command_line, Line}, {client, Client}]),
      proceed
  end,
  {noreply, State};

handle_info({result, {IP, Port} = _RoutingHint, Line} = _Msg,
            State = #state{socket = Socket}) ->
  gen_udp:send(Socket, IP, Port, [Line, "\n"]),
  {noreply, State};

handle_info(_Msg, State = #state{}) ->
  {noreply, State}. % ignore other messages


%%%---------------------------------------------------------------------------
%%% network helpers
%%%---------------------------------------------------------------------------

%% @doc Resolve DNS address to IP.
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
