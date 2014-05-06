%%%---------------------------------------------------------------------------
%%% @doc
%%%   Module that handles single connection.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_af_unix_connection).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% API for server socket
-export([start_monitor/0, c_data/2, c_eof/1]).

%% client socket API
-export([send/2, recv/1, recv/2, controlling_process/2, close/1]).

%%%---------------------------------------------------------------------------

-record(state, {
  parent,
  fd,
  owner = undefined,
  data_queue = queue:new(),
  eof = false
}).

%%%---------------------------------------------------------------------------

%% @type connection_socket() = indira_af_unix:connection_socket().

-type connection_socket() :: indira_af_unix:connection_socket().

%%%---------------------------------------------------------------------------
%%% API for server socket
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start new connection handler process and monitor it.
%%
%% @spec start_monitor() ->
%%   {ok, connection_socket(), reference()}

-spec start_monitor() ->
  {ok, connection_socket(), reference()}.

start_monitor() ->
  % R14: I wish I could pass `{spawn_opt, [monitor]}'
  {ok, Pid} = gen_server:start(?MODULE, self(), []),
  Ref = erlang:monitor(process, Pid),
  {ok, Pid, Ref}.

%% @private
%% @doc Dispatch data according to controlling process.
%%
%% @spec c_data(connection_socket(), binary()) ->
%%   ok

c_data(Connection, Data) ->
  gen_server:cast(Connection, {incoming, Data}).

%% @private
%% @doc Mark the connection as closed.
%%
%% @spec c_eof(connection_socket()) ->
%%   ok

c_eof(Connection) ->
  gen_server:cast(Connection, {eof}).

%%%---------------------------------------------------------------------------
%%% client socket API
%%%---------------------------------------------------------------------------

%% @doc Send data to the client.
%%
%% @spec send(connection_socket(), iolist()) ->
%%   ok

send(Connection, Data) ->
  SurelyIOList = iolist_to_binary(Data),
  gen_server:cast(Connection, {outgoing, SurelyIOList}).

%% @doc Receive all (currently available) data from client.
%%
%% @spec recv(connection_socket()) ->
%%   {ok, binary()} | {error, Reason}

recv(Connection) ->
  recv(Connection, 0).

%% @doc Receive exactly `Size' data from client.
%%
%% @TODO Implement this.
%%
%% @spec recv(connection_socket(), non_neg_integer()) ->
%%   {ok, binary()} | {error, Reason}

recv(Connection, Size) ->
  gen_server:call(Connection, {recv, Size}).

%% @doc Set process that will receive data as messages.
%%   This automatically sets the connection as an active.
%%
%% @spec controlling_process(connection_socket(), pid()) ->
%%   ok

controlling_process(Connection, Pid) ->
  gen_server:cast(Connection, {controlling_process, Pid}).

%% @doc Close the connection.
%%
%% @spec close(connection_socket()) ->
%%   ok

close(Connection) ->
  gen_server:cast(Connection, {close}).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%--------------------------------------------------------------------
%% initialization and termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init(Parent) ->
  Ref = erlang:monitor(process, Parent),
  State = #state{parent = {Parent, Ref}},
  {ok, State}.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Reason, _State) ->
  % no cleanup needed, parent should close the connection
  ok.

%% }}}
%%--------------------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

%% recv request, active mode
handle_call({recv, _} = _Request, _From, State = #state{owner = {_,_}}) ->
  {reply, {error, active_mode}, State};

%% recv request, passive mode
%% TODO: support for non-zero size
handle_call({recv, _} = _Request, _From,
            State = #state{data_queue = Queue, owner = undefined}) ->
  case {queue:is_empty(Queue),State#state.eof} of
    {true,true} ->
      % EOF, no more data to return -> exit
      {stop, normal, {error,closed}, State};
    {true,false} ->
      % not EOF yet
      {reply, {ok, <<>>}, State};
    {false,_Any} ->
      % some data to return (current queue goes empty)
      Data = iolist_to_binary(queue:to_list(Queue)),
      {reply, {ok,Data}, State#state{data_queue = queue:new()}}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

%% new controlling process
handle_cast({controlling_process, NewOwnerPid} = _Request, State) ->
  case State of
    % no previous owner
    #state{owner = undefined} -> ok;
    % already had an owner
    #state{owner = {_OldOwnerPid,Ref}} -> erlang:demonitor(Ref, [flush])
  end,
  NewRef = erlang:monitor(process, NewOwnerPid),
  NewState = send_all_queued_data(State#state{owner = {NewOwnerPid,NewRef}}),
  case NewState of
    #state{eof = true} ->
      NewOwnerPid ! {unix_closed, self()},
      {stop, normal, NewState};
    #state{eof = false} ->
      {noreply, NewState}
  end;

%% data from socket, passive mode
handle_cast({incoming, Payload} = _Request,
            State = #state{owner = undefined, data_queue = Queue}) ->
  NewQueue = queue:in(Payload, Queue),
  {noreply, State#state{data_queue = NewQueue}};

%% data from socket, active mode
handle_cast({incoming, Payload} = _Request,
            State = #state{owner = {OwnerPid,_Ref}}) ->
  % TODO: split this and send line by line
  OwnerPid ! {unix, self(), Payload},
  {noreply, State};

%% EOF from socket, passive mode
handle_cast({eof} = _Request, State = #state{owner = undefined}) ->
  % just mark this socket as EOF, some later call will terminate the process
  {noreply, State#state{eof = true}};

%% EOF from socket, active mode
handle_cast({eof} = _Request, State = #state{owner = {OwnerPid,_Ref}}) ->
  OwnerPid ! {unix_closed, self()},
  {stop, normal, State};

%% data to socket (already closed)
handle_cast({outgoing, _Payload} = _Request, State = #state{eof = true}) ->
  % just ignore
  % TODO: or maybe `{stop, eof, State}'?
  {noreply, State};

%% data to socket
handle_cast({outgoing, Payload} = _Request,
            State = #state{parent = {ParentPid,_Ref}, eof = false}) ->
  indira_af_unix:c_send(ParentPid, Payload),
  {noreply, State};

%% close request from reader process
handle_cast({close} = _Request, State) ->
  % parent monitors this process, so it's just enough to stop
  {stop, normal, State};

handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
%% @doc Handle incoming messages.

%% server socket closed, active mode
handle_info({'DOWN', Ref, process, ParentPid, _Reason} = _Message,
            State = #state{owner = {_,_}, parent = {ParentPid, Ref}}) ->
  {stop, normal, State};

%% server socket closed, passive mode
handle_info({'DOWN', Ref, process, ParentPid, _Reason} = _Message,
            State = #state{owner = undefined, parent = {ParentPid, Ref}}) ->
  {noreply, State#state{eof = true}};

%% owner shut down, active mode (there's no owner in passive mode)
handle_info({'DOWN', Ref, process, OwnerPid, _Reason} = _Message,
            State = #state{owner = {OwnerPid, Ref}}) ->
  {stop, normal, State};

handle_info(_Message, State) ->
  {noreply, State}.

%% }}}
%%--------------------------------------------------------------------
%% code change {{{

%% @private
%% @doc Handle code change.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% }}}
%%--------------------------------------------------------------------

%% @doc Flush the data queue.
%%   This sends all pending data from the queue to current connection owner.
%%
%% @spec send_all_queued_data(#state{}) ->
%%   #state{}

send_all_queued_data(State = #state{owner = {Pid,_Ref}, data_queue = Queue}) ->
  NewQueue = send_queue(Pid, Queue),
  State#state{data_queue = NewQueue}.

%% @doc Send every message from `Queue' to `Pid'.
%%
%% @spec send_queue(pid(), term()) ->
%%   term()

send_queue(Pid, Queue) ->
  case queue:out(Queue) of
    {empty, NewQueue} ->
      NewQueue;
    {{value,M}, NewQueue} ->
      Pid ! {unix, self(), M},
      send_queue(Pid, NewQueue)
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
