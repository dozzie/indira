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
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_udp).

-behaviour(gen_indira_listener).
-behaviour(gen_server).

%%%---------------------------------------------------------------------------

%% Indira listener API
-export([child_spec/1]).
-export([send_one_line/3, retry_send_one_line/3]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% API for supervision tree
-export([start_link/2]).

%%%---------------------------------------------------------------------------

-include_lib("kernel/include/inet.hrl").

-record(state, {socket}).

%%%---------------------------------------------------------------------------
%%% Indira listener API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Listener description.

child_spec({Host, Port} = _Args) ->
  {ignore,
    {?MODULE, start_link, [Host, Port]},
    permanent, 5000, worker, [?MODULE]}.

%% @private
%% @doc Send a line to socket.

-spec send_one_line({inet:hostname() | inet:ip_address(), inet:port_number()},
                    iolist(), timeout()) ->
  {ok, iolist()} | {error, term()}.

send_one_line({Addr, Port} = _Address, Line, Timeout) ->
  case gen_udp:open(0, [{active, false}, binary]) of
    {ok, Sock} ->
      case gen_udp:send(Sock, Addr, Port, [Line, $\n]) of
        ok ->
          case gen_udp:recv(Sock, 0, Timeout) of
            % XXX: this will accept anything that was sent to this socket,
            % even if it didn't come from Addr:Port
            {ok, {_Addr, _Port, ReplyLine}} ->
              gen_udp:close(Sock),
              {ok, ReplyLine};
            {error, Reason} ->
              gen_udp:close(Sock),
              {error, Reason}
          end;
        {error, Reason} ->
          gen_udp:close(Sock),
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @private
%% @doc Send a line to socket, retrying when socket refuses connections.
%%   Hell, it's UDP. It doesn't refuse connections in any reliable way, so
%%   just do the same as {@link send_one_line/3}.

retry_send_one_line(Address, Line, Timeout) ->
  send_one_line(Address, Line, Timeout).

%%%---------------------------------------------------------------------------
%%% public API for supervision tree
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start UDP listener process.

start_link(Host, Port) ->
  gen_server:start_link(?MODULE, [Host, Port], []).

%%%---------------------------------------------------------------------------
%%% connection acceptor
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization and termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.
%%   This includes creating listening UDP socket.

init([Host, Port] = _Args) ->
  % create listening socket
  BindOpt = address_to_bind_option(Host),
  {ok, Socket} = gen_udp:open(Port, BindOpt ++ [
    {active, true}, {reuseaddr, true}, list
  ]),
  State = #state{socket = Socket},
  {ok, State}.

%% @private
%% @doc Clean up {@link gen_server} state.
%%   This includes closing the listening socket.

terminate(_Reason, _State = #state{socket = Socket}) ->
  gen_udp:close(Socket),
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
  {noreply, State}. % ignore unknown casts

%% @private
%% @doc Handle incoming messages (UDP data and commands).

handle_info({udp, Socket, IP, Port, Line} = _Msg,
            State = #state{socket = Socket}) ->
  RoutingHint = {IP, Port},
  gen_indira_listener:command(RoutingHint, Line),
  {noreply, State};

handle_info({result, {IP, Port} = _RoutingHint, Line} = _Msg,
            State = #state{socket = Socket}) ->
  gen_udp:send(Socket, IP, Port, [Line, "\n"]),
  {noreply, State};

handle_info(_Msg, State = #state{}) ->
  {noreply, State}. % ignore other messages

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
%%% network helpers
%%%---------------------------------------------------------------------------

%% @doc Resolve DNS address to IP.

-spec address_to_bind_option(any | inet:hostname() | inet:ip_address()) ->
  [{atom(), term()}].

address_to_bind_option(any) ->
  [];
address_to_bind_option(Addr) when is_list(Addr); is_atom(Addr) ->
  % TODO: IPv6 (inet6)
  {ok, HostAddr} = inet:getaddr(Addr, inet),
  [{ip, HostAddr}];
address_to_bind_option(Addr) when is_tuple(Addr) ->
  [{ip, Addr}].

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
