%%%---------------------------------------------------------------------------
%%% @doc
%%%   Module that imitates multiple connections.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_af_unix).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% server socket API
-export([listen/1, accept/1, accept/2, close/1]).

%% API for clients
-export([c_send/2]).

-export_type([server_socket/0, connection_socket/0]).

%%%---------------------------------------------------------------------------

-define(POLL_INTERVAL, 100).

-record(state, {
  port,  % port driver handle
  owner, % process to die with
  fd_queue = queue:new(), % FDs waiting for accept()
  fd  = dict:new(), % FD -> PID
  pid = dict:new()  % PID -> FD
}).

%%%---------------------------------------------------------------------------

%% @type server_socket() = pid().
%%   Server socket handler (process running this module).

-type server_socket() :: pid().

%% @type connection_socket() = pid().
%%   Client connection handler (process running
%%   {@link indira_af_unix_connection}).

-type connection_socket() :: pid().

%%%---------------------------------------------------------------------------
%%% API for clients
%%%---------------------------------------------------------------------------

%% @private
%% @doc Send payload to appropriate unix socket.
%%
%% @spec c_send(server_socket(), iolist()) ->
%%   ok

-spec c_send(server_socket(), iolist()) ->
  ok.

c_send(SockPid, Payload) ->
  gen_server:cast(SockPid, {send, self(), Payload}).

%%%---------------------------------------------------------------------------
%%% server socket API
%%%---------------------------------------------------------------------------

%% @doc Setup a socket listening on specified address.
%%
%% @spec listen(string()) ->
%%   {ok, server_socket()}

-spec listen(string()) ->
  {ok, server_socket()}.

listen(Address) ->
  case gen_server:start(?MODULE, {Address, self()}, []) of
    {ok, Pid} ->
      link(Pid),
      {ok, Pid};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Accept a client connection.
%%   The function waits infinitely for a client.
%%
%%   To work with returned socket, see {@link indira_af_unix_connection}.
%%
%% @spec accept(server_socket()) ->
%%   {ok, connection_socket()} | {error, Reason}

-spec accept(server_socket()) ->
  {ok, connection_socket()} | {error, term()}.

accept(SockPid) ->
  case gen_server:call(SockPid, {accept}) of
    {ok, Pid} -> {ok, Pid};
    nothing ->
      timer:sleep(50),
      accept(SockPid);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Accept a client connection.
%%
%%   To work with returned socket, see {@link indira_af_unix_connection}.
%%
%% @spec accept(server_socket(), non_neg_integer()) ->
%%   {ok, connection_socket()} | {error, timeout} | {error, Reason}

-spec accept(server_socket(), non_neg_integer()) ->
  {ok, connection_socket()} | {error, timeout} | {error, term()}.

accept(_SockPid, Timeout) when Timeout =< 0 ->
  {error, timeout};

accept(SockPid, Timeout) when Timeout > 0 ->
  Sleep = if
    Timeout >= 50 -> 50;
    Timeout < 50 -> Timeout
  end,
  case gen_server:call(SockPid, {accept}) of
    {ok, Pid} -> {ok, Pid};
    nothing ->
      timer:sleep(Sleep),
      accept(SockPid, Timeout - Sleep);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Close server connection.
%%
%% @spec close(server_socket()) ->
%%   ok

-spec close(server_socket()) ->
  ok.

close(SockPid) ->
  gen_server:call(SockPid, {close}).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%--------------------------------------------------------------------
%% initialization and termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init({Address, OwnerPid} = _Args) ->
  case indira_af_unix_driver:listen(Address) of
    {ok, Port} ->
      Ref = erlang:monitor(process, OwnerPid),
      State = #state{port = Port, owner = {OwnerPid, Ref}},
      {ok, State, ?POLL_INTERVAL};
    {error, Reason} ->
      {stop, Reason}
  end.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Reason, _State = #state{port = Port}) ->
  indira_af_unix_driver:close(Port),
  ok.

%% }}}
%%--------------------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

handle_call({accept} = _Request, _From, State) ->
  NewState = poll(State),
  case queue:out(NewState#state.fd_queue) of
    {{value,V}, NewQueue} ->
      {reply, {ok, V}, NewState#state{fd_queue = NewQueue}, ?POLL_INTERVAL};
    {empty, NewQueue} ->
      {reply, nothing, NewState#state{fd_queue = NewQueue}, ?POLL_INTERVAL}
  end;

handle_call({close} = _Request, _From, State) ->
  {stop, normal, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State, ?POLL_INTERVAL}.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

handle_cast({send, ConnPid, Data} = _Request,
            State = #state{port = Port, pid = PIDMap}) ->
  case dict:find(ConnPid, PIDMap) of
    {ok, FD} -> indira_af_unix_driver:send(Port, FD, Data);
    error    -> ignore
  end,
  {noreply, State, ?POLL_INTERVAL};

handle_cast(_Request, State) ->
  {noreply, State, ?POLL_INTERVAL}.

%% @private
%% @doc Handle incoming messages.

handle_info(timeout = _Message, State) ->
  NewState = poll(State),
  {noreply, NewState, ?POLL_INTERVAL};

%% socket owner went down
handle_info({'DOWN', Ref, process, OwnerPid, Reason} = _Message,
            State = #state{owner = {OwnerPid, Ref}}) ->
  {stop, Reason, State};

%% client connection was closed
handle_info({'DOWN', _Ref, process, Pid, _Reason} = _Message,
            State = #state{port = Port, fd = FDMap, pid = PIDMap}) ->
  case dict:find(Pid, PIDMap) of
    {ok, FD} ->
      indira_af_unix_driver:close(Port, FD),
      NewFDMap = dict:erase(FD, FDMap),
      NewPIDMap = dict:erase(Pid, PIDMap),
      NewState = State#state{fd = NewFDMap, pid = NewPIDMap},
      {noreply, NewState, ?POLL_INTERVAL};
    error ->
      % ignore this message
      {noreply, State, ?POLL_INTERVAL}
  end;

handle_info(_Message, State) ->
  {noreply, State, ?POLL_INTERVAL}.

%% }}}
%%--------------------------------------------------------------------
%% code change {{{

%% @private
%% @doc Handle code change.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% }}}
%%--------------------------------------------------------------------

%%%---------------------------------------------------------------------------

%% @doc Check `Port' for any events and dispatch them accordingly.
%%   For any new descriptor there will be {@link indira_af_unix_connection}
%%   spawned and monitored.
%%
%%   Function returns new file descriptors and updated process state.
%%
%% @spec poll(#state{}) ->
%%   #state{}

poll(State = #state{port = Port}) ->
  % Events :: {new, FD}
  %         | {data, FD::integer(), binary()}
  %         | {close, FD}
  Events = indira_af_unix_driver:poll(Port),
  NewState = events_dispatch(Events, State),
  NewState.

%% @doc Dispatch events to appropriate child processes.
%%
%% @spec events_dispatch([indira_af_unix_driver:event()], #state{}) ->
%%   #state{}

events_dispatch([] = _Events, State) ->
  State;

events_dispatch([{new, FD} | Rest] = _Events,
                State = #state{fd = FDMap, pid = PIDMap, fd_queue = Queue}) ->
  {ok, Pid, MonRef} = indira_af_unix_connection:start_monitor(),
  NewFDMap = dict:store(FD, {Pid,MonRef}, FDMap),
  NewPIDMap = dict:store(Pid, FD, PIDMap),
  NewQueue = queue:in(Pid, Queue),
  NewState = State#state{fd = NewFDMap, pid = NewPIDMap, fd_queue = NewQueue},
  events_dispatch(Rest, NewState);

events_dispatch([{data, FD, Data} | Rest] = _Events,
                State = #state{fd = FDMap}) ->
  {ok, {Pid,_MonRef}} = dict:find(FD, FDMap),
  indira_af_unix_connection:c_data(Pid, Data),
  events_dispatch(Rest, State);

events_dispatch([{close, FD} | Rest] = _Events,
                State = #state{fd = FDMap, pid = PIDMap}) ->
  {ok, {Pid,MonRef}} = dict:find(FD, FDMap),
  erlang:demonitor(MonRef, [flush]),
  indira_af_unix_connection:c_eof(Pid),
  NewFDMap = dict:erase(FD, FDMap),
  NewPIDMap = dict:erase(Pid, PIDMap),
  NewState = State#state{fd = NewFDMap, pid = NewPIDMap},
  events_dispatch(Rest, NewState).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
