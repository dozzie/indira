%%%---------------------------------------------------------------------------
%%% @doc
%%%   Stream socket (listening socket) behaviour.
%%%   This module was copied from `gen_sock_stream' and included in Indira to
%%%   lower its dependency profile.
%%%
%%%   Stream socket is a socket with an idea of <i>connection</i> and
%%%   <i>listen</i> state.
%%%
%%%   == Expected callbacks ==
%%%
%%%   <ul>
%%%     <li>
%```
%listen(Address) ->
%    {ok, State :: term()}
%  | ignore
%  | {error, Reason :: term()}
%'''
%%%       `State' contains listening socket.
%%%     </li>
%%%     <li>
%```
%accept(State) ->
%    {ok, Connection, State}
%  | {ok, State}
%  | {stop, Reason, State}
%'''
%%%       `Connection' will be passed as an argument to connection handler
%%%       module.
%%%
%%%       It's a good idea for this function to return after some specified
%%%       time passed (e.g. after 100ms), so the process can receive and
%%%       handle incoming messages and calls.
%%%     </li>
%%%     <li>
%```
%controlling_process(Connection, Pid) ->
%    ok
%  | {error, Reason}
%'''
%%%       Make `Pid' to be the controlling process ("owner") of the
%%%       `Connection'.
%%%     </li>
%%%     <li>
%```
%close(State) ->
%  any()
%'''
%%%     </li>
%%%   </ul>
%%%
%%%  == Connection handler module ==
%%%
%%%  Connection handler can be any module. Especially well suited are modules
%%%  implementing {@link gen_server} or {@link gen_event} behaviours.
%%%
%%%  Such module should only export a function that spawns new linked process
%%%  (default name for such function is `start_link').
%%%
%%%  The function will be called with {@type connection()} (returned by
%%%  `ListenModule:accept/1') as its only argument. The function should return
%%%  `{ok,pid()}', `ignore' or `{error,Reason}' (returned values from {@link
%%%  gen_server:start_link/3} are fine here). It is this function's
%%%  responsibility to close passed {@type connection()} in case of `ignore'
%%%  or `{error,Reason}'.
%%%
%%%  === Small code example ===
%%%
%%%  Starting (unsupervised) `my_connection_listener' spawning
%%%  `my_connection_handler' (TCP listener not shown here for brevity):
%%%  ```
%%%  gen_indira_sock_stream:start_link(
%%%    my_connection_listener,
%%%    {my_connection_handler,start_link}, % or just `my_connection_handler'
%%%    {"localhost", 12345}
%%%  ).
%%%  '''
%%%
%%%  TCP connection handler:
%%%  ```
%%%  -module(my_connection_handler).
%%%  -behaviour(gen_server).
%%%
%%%  -export([start_link/1]).
%%%  % ...
%%%
%%%  start_link(Connection) ->
%%%    case gen_server:start_link(?MODULE, Connection, []) of
%%%      {ok, Pid} ->
%%%        {ok, Pid};
%%%      Failure ->
%%%        gen_tcp:close(Connection),
%%%        Failure
%%%    end.
%%%  % ...
%%%  '''
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_sock_stream).

-behaviour(gen_server).

%% behaviour description (pre-R15)
-export([behaviour_info/1]).

%% public API
-export([start_link/3, supervisor/3]).

%% private API
-export([start_link_supervised/4]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% types for callback modules
-export_type([listen_address/0]).
-export_type([listen_socket/0, connection/0]).
-export_type([listener_module/0, connection_handler/0]).

%%%---------------------------------------------------------------------------

-record(state, {
  socket,
  listen_module,
  connection_handler,
  supervisor
}).

%%%---------------------------------------------------------------------------

%% @type listen_address() = term().
%%   Address specification for listener to listen on.

-type listen_address() :: term().

%% @type listen_socket() = term().
%%   Listening socket handle.

-type listen_socket() :: term().

%% @type connection() = term().
%%   Connection handle.

-type connection() :: term().

%% @type connection_handler() =
%%     atom() | {Module :: atom(), Function :: atom()}.
%%
%%   Specification for module handling single {@type connection()}.
%%
%%   If specified as a module name, the function name defaults to
%%   `start_link'.
%%
%%   See {@section Connection handler module}.

-type connection_handler() :: atom() | {atom(), atom()}.

%% @type listener_module() = atom().
%%   Name of the module used to create {@type listen_socket()} and to accept
%%   incoming connections. The module is expected to implement
%%   `gen_indira_sock_stream' behaviour (see {@section Expected callbacks}).

-type listener_module() :: atom().

%%%---------------------------------------------------------------------------
%%% behaviour description
%%%---------------------------------------------------------------------------

%% @doc Behaviour description.

behaviour_info(callbacks = _Item) ->
  [{listen, 1}, {accept, 1}, {close, 1}, {controlling_process, 2}];
behaviour_info(_Item) ->
  undefined.

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%% @doc Start listener process outside of supervision tree.
%%
%%   `ListenModule' is expected to implement `gen_indira_sock_stream'
%%   behaviour (see {@section Expected callbacks}).
%%
%%   `ConnHandlerModule' specifies module to handle single connection in
%%   a separate process. Such process is started by function part in
%%   `ConnHandlerModule' (or `start_link/1' if it's not specified).
%%   See {@type connection_handler()} for details.
%%
%% @spec start_link(listener_module(), connection_handler(),
%%                  listen_address()) ->
%%   {ok, pid()} | ignore | {error, Reason}

-spec start_link(listener_module(), connection_handler(), listen_address()) ->
  {ok, pid()} | ignore | {error, term()}.

start_link(ListenModule, ConnHandlerModule, Address) ->
  Argument = {
    ListenModule,
    case ConnHandlerModule of
      {_Mod,_Fun} -> ConnHandlerModule;
      _ when is_atom(ConnHandlerModule) -> {ConnHandlerModule, start_link}
    end,
    Address
  },
  gen_server:start_link(?MODULE, Argument, []).

%% @doc Start supervision (sub)tree for listener process.
%%   This function should be used instead of {@link start_link/3} if the
%%   listener is meant to be a part of supervision tree.
%%
%%   `ListenModule' is expected to implement `gen_indira_sock_stream'
%%   behaviour (see {@section Expected callbacks}).
%%
%%   `ConnHandlerModule' specifies module to handle single connection in
%%   a separate process. Such process is started by function part in
%%   `ConnHandlerModule' (or `start_link/1' if it's not specified).
%%   See {@type connection_handler()} for details.
%%
%% @spec supervisor(listener_module(), connection_handler(), term()) ->
%%   {ok, Supervisor :: pid()} | ignore | {error, Reason}

supervisor(ListenModule, ConnHandlerModule, Address) ->
  case ConnHandlerModule of
    {Mod,Fun} -> ok;
    Mod when is_atom(Mod) -> Fun = start_link
  end,
  indira_sock_stream_sup:start_link(ListenModule, {Mod,Fun}, Address).

%%%---------------------------------------------------------------------------
%%% private API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start listener process inside of supervision tree.
%%
%%   `ListenModule' is expected to implement `gen_indira_sock_stream'
%%   behaviour (see {@section Expected callbacks}).
%%
%%   `ConnHandler' is expected to have function name explicit.
%%
%% @spec start_link_supervised(pid(), listener_module(), connection_handler(),
%%                             term()) ->
%%   {ok, pid()} | ignore | {error, Reason}

-spec start_link_supervised(pid(), listener_module(), connection_handler(),
                            term()) ->
  {ok, pid()} | ignore | {error, term()}.

start_link_supervised(Supervisor, ListenModule, ConnHandler, Address) ->
  Argument = {
    Supervisor,
    ListenModule,
    ConnHandler,
    Address
  },
  gen_server:start_link(?MODULE, Argument, []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%--------------------------------------------------------------------
%% initialization and termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init({ListenModule, ConnHandlerModule, Address} = _Args) ->
  case ListenModule:listen(Address) of
    {ok, Socket} ->
      State = #state{
        socket = Socket,
        listen_module = ListenModule,
        connection_handler = ConnHandlerModule
      },
      {ok, State, 0};
    ignore ->
      ignore;
    {error, Reason} ->
      {stop, Reason}
  end;

init({Supervisor, ListenModule, ConnHandlerModule, Address} = _Args) ->
  case ListenModule:listen(Address) of
    {ok, Socket} ->
      State = #state{
        socket = Socket,
        listen_module = ListenModule,
        connection_handler = ConnHandlerModule
      },
      self() ! {get_connection_supervisor, Supervisor},
      {ok, State}; % no need to specify timeout
    ignore ->
      ignore;
    {error, Reason} ->
      {stop, Reason}
  end.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Reason, _State = #state{socket = Socket, listen_module = Module}) ->
  Module:close(Socket).

%% }}}
%%--------------------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

handle_call(_Request, _From, State) ->
  {reply, {error, invalid_request}, State, 0}.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

handle_cast(_Request, State) ->
  {noreply, State, 0}.

%% @private
%% @doc Handle incoming messages.

handle_info(timeout = _Message,
            State = #state{socket = Socket, listen_module = ListenModule}) ->
  case ListenModule:accept(Socket) of
    {ok, NewSocket} ->
      {noreply, State#state{socket = NewSocket}, 0};

    {ok, Connection, NewSocket} ->
      case spawn_handler(Connection, State) of
        {ok, Pid} ->
          ListenModule:controlling_process(Connection, Pid),
          ok;
        failed ->
          ignore
      end,
      {noreply, State#state{socket = NewSocket}, 0};

    {stop, Reason, NewSocket} ->
      {stop, Reason, State#state{socket = NewSocket}}
  end;

handle_info({get_connection_supervisor, Supervisor} = _Message, State) ->
  {ok, Pid} = indira_sock_stream_sup:start_connection_supervisor(Supervisor),
  {noreply, State#state{supervisor = Pid}, 0};

handle_info(_Message, State) ->
  {noreply, State, 0}.

%% }}}
%%--------------------------------------------------------------------
%% code change {{{

%% @private
%% @doc Handle code change.

code_change(_OldVsn, State, _Extra) ->
  % FIXME: how about `State#state.listen_module'?
  {ok, State}.

%% }}}
%%--------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% helpers
%%%---------------------------------------------------------------------------

%%--------------------------------------------------------------------
%% spawn connection handler process {{{

%% @doc Spawn connection handler process.

%% not supervised
spawn_handler(Connection, State = #state{supervisor = undefined}) ->
  case State of
    #state{connection_handler = {Mod,Fun}} ->
      ok;
    #state{connection_handler = Mod} when is_atom(Mod) ->
      Fun = start_link
  end,

  Self = self(),
  Ref = make_ref(),

  spawn(fun() ->
    process_flag(trap_exit, true), % don't die on abrupt termination
    % FIXME: this can hang forever if the `Mod:Fun/1' is badly written
    case Mod:Fun(Connection) of
      {ok, Pid}        -> Self ! {Ref, pid, Pid};
      ignore           -> Self ! {Ref, failed};
      {error, _Reason} -> Self ! {Ref, failed} % TODO: log this
    end
  end),

  receive
    {Ref, pid, Pid} ->
      {ok, Pid};
    {Ref, ignore} ->
      failed
  end;

%% supervised
spawn_handler(Connection, _State = #state{supervisor = Supervisor}) ->
  case supervisor:start_child(Supervisor, [Connection]) of
    {ok, Child} ->
      {ok, Child};
    {ok, Child, _Info} ->
      {ok, Child};
    {error, _Reason} ->
      failed
  end.

%% }}}
%%--------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
