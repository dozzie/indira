%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Helper manager for AF_UNIX: port driver loader and port owner of
%%%   listening socket deleter.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_af_unix_manager).

-behaviour(gen_server).

%% public interface
-export([watch/1]).

%% supervision tree API
-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------
%%% types {{{

-record(state, {
  sockets :: ets:tab(), % port() -> Device, Inode, Path
  unlinker :: port()
}).

%%% }}}
%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start AF_UNIX manager process.

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @private
%% @doc Start AF_UNIX manager process.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Register a socket for unlinking after BEAM crash.

-spec watch(indira_af_unix:server_socket()) ->
  ok.

watch(Socket) ->
  {ok, {Device, Inode, ListenPath}} = indira_af_unix:stat(Socket),
  {ok, CWD} = file:get_cwd(),
  AbsPath = filename:join(CWD, ListenPath),
  gen_server:call(?MODULE, {register, Socket, {Device, Inode}, AbsPath}).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init(_Args) ->
  case start_unlinker() of
    {ok, Port} ->
      indira_af_unix:load_port_driver(),
      process_flag(trap_exit, true),
      Table = ets:new(sockets, [set]),
      State = #state{
        sockets = Table,
        unlinker = Port
      },
      {ok, State};
    {error, bad_name} ->
      {stop, {missing_application, indira}}
  end.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Arg, _State = #state{unlinker = Port, sockets = Table}) ->
  indira_af_unix:unload_port_driver(),
  ets:delete(Table),
  stop_unlinker(Port),
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

handle_call({register, Socket, {_,_} = StatInfo, SocketPath} = _Request, _From,
            State = #state{unlinker = Port, sockets = Table}) ->
  try link(Socket) of
    _ ->
      ets:insert(Table, {Socket, StatInfo, SocketPath}),
      add_path(Port, StatInfo, SocketPath),
      {reply, ok, State}
  catch
    error:Reason ->
      {reply, {error, Reason}, State}
  end;

%% unknown calls
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

%% unknown casts
handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
%% @doc Handle incoming messages.

handle_info({Port, {exit_status, Status}} = _Message,
            State = #state{unlinker = Port}) ->
  {stop, {unlinker_exit, Status}, State};

handle_info({'EXIT', Port, Reason} = _Message,
            State = #state{unlinker = Port}) ->
  {stop, Reason, State};

handle_info({'EXIT', Socket, _Reason} = _Message,
            State = #state{unlinker = Port, sockets = Table}) ->
  case ets:lookup(Table, Socket) of
    [{Socket, StatInfo, _SocketPath}] ->
      ets:delete(Table, Socket),
      forget_path(Port, StatInfo),
      {noreply, State};
    [] ->
      {noreply, State}
  end;

%% unknown messages
handle_info(_Message, State) ->
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

%% @doc Start unlinker process.

-spec start_unlinker() ->
  {ok, port()} | {error, bad_name | system_limit | inet:posix()}.

start_unlinker() ->
  case code:lib_dir(indira, priv) of
    {error, bad_name} ->
      {error, bad_name};
    PrivDir ->
      ExePath = filename:join(PrivDir, "indira_unix_unlinker"),
      try open_port({spawn_executable, ExePath}, [{packet, 2}, exit_status]) of
        Port ->
          MaxPorts = max_ports(),
          port_command(Port, <<MaxPorts:32>>),
          {ok, Port}
      catch
        error:Reason ->
          {error, Reason}
      end
  end.

%% @doc Stop unlinker process.

-spec stop_unlinker(port()) ->
  any().

stop_unlinker(Port) ->
  try
    port_close(Port)
  catch
    _:_ -> ignore
  end.

%% @doc Add socket path to unlinking on BEAM crash.

-spec add_path(port(), {indira_af_unix:device(), indira_af_unix:inode()},
               file:filename()) ->
  any().

add_path(Port, {Device, Inode} = _StatInfo, Path) ->
  port_command(Port, [<<"+", Device:64, Inode:64>>, Path]).

%% @doc Remove socket path from unlinking on BEAM crash.

-spec forget_path(port(), {indira_af_unix:device(), indira_af_unix:inode()}) ->
  any().

forget_path(Port, {Device, Inode} = _StatInfo) ->
  port_command(Port, <<"-", Device:64, Inode:64>>).

%%%---------------------------------------------------------------------------

%% @doc Get maximum number of ports that BEAM can keep.

-spec max_ports() ->
  pos_integer().

max_ports() ->
  try
    % Erlang R16 and later
    erlang:system_info(port_limit)
  catch
    error:badarg ->
      % before Erlang R15
      case os:getenv("ERL_MAX_PORTS") of
        false ->
          1024; % R15 default
        Value ->
          try list_to_integer(Value) of
            MaxPorts when MaxPorts >= 1024 -> MaxPorts;
            _ -> 1024
          catch
            error:badarg -> 1024
          end
      end
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
