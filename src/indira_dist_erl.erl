%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Distributed Erlang configurator and deconfigurator.
%%%
%%%   This process is intended for configuring Erlang networking in
%%%   a (possibly) delayed manner. Networking can be established and shut down
%%%   as necessary, instead of keeping it running all the time.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_dist_erl).

-behaviour(gen_server).

%% public interface
-export([bring_up/0, tear_down/0, read_cookie/1]).

%% supervision tree API
-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {
  config :: net_config()
}).

-type cookie() :: atom() | {file, file:filename()} | none.

-type net_config() :: {node(), shortnames | longnames, cookie()}.

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Start Erlang networking.

-spec bring_up() ->
  ok | {error, term()}.

bring_up() ->
  gen_server:call(?MODULE, start).

%% @doc Shutdown Erlang networking.

-spec tear_down() ->
  ok | {error, term()}.

tear_down() ->
  gen_server:call(?MODULE, stop).

%% @doc Read a magic cookie from a specified file.
%%   The cookie is the first line of the file.

-spec read_cookie(file:filename()) ->
  {ok, atom()} | {error, term()}.

read_cookie(Filename) ->
  case file:open(Filename, [read, raw, binary]) of
    {ok, FH} ->
      ReadResult = file:read_line(FH),
      file:close(FH),
      case ReadResult of
        {ok, Line} ->
          case binary:split(Line, <<"\n">>, [trim]) of
            [LineNoNL] when size(LineNoNL) > 0 ->
              {ok, binary_to_atom(LineNoNL, utf8)};
            [<<>>] ->
              {error, empty_file}
          end;
        eof ->
          {error, empty_file};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start configurator process.

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @private
%% @doc Start configurator process.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init(_Args) ->
  indira_log:set_context(erlang_networking, []),
  {NetInitialStatus, NetConfig} = get_network_config(),
  State = #state{
    config = NetConfig
  },
  case NetInitialStatus of
    started ->
      case start_network(NetConfig) of
        ok ->
          {ok, State};
        {error, Reason} ->
          indira_log:crit("can't configure Erlang networking",
                          [{error, Reason}]),
          {stop, {net_error, Reason}}
      end;
    stopped ->
      {ok, State}
  end.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Arg, _State = #state{}) ->
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

handle_call(start = _Request, _From, State = #state{config = undefined}) ->
  Reply = case node() of
    nonode@nohost -> {error, not_configured};
    _ -> ok % networking already configured and started
  end,
  {reply, Reply, State};

handle_call(start = _Request, _From, State = #state{config = NetConfig}) ->
  case start_network(NetConfig) of
    ok = Reply ->
      ok;
    {error, Reason} = Reply ->
      indira_log:info("can't configure Erlang networking", [{error, Reason}])
  end,
  {reply, Reply, State};

handle_call(stop = _Request, _From, State) ->
  case stop_network() of
    ok = Reply ->
      ok;
    {error, Reason} = Reply ->
      indira_log:info("can't shutdown Erlang networking", [{error, Reason}])
  end,
  {reply, Reply, State};

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

%% @doc Start Erlang's networking.
%%   This includes setting cookie, if one was provided.

-spec start_network(net_config()) ->
  ok | {error, term()}.

start_network({Node, NameType, {file, CookieFile}} = _NetConfig) ->
  case read_cookie(CookieFile) of
    {ok, Cookie} -> start_network({Node, NameType, Cookie});
    {error, Reason} -> {error, {read_cookie, Reason}}
  end;
start_network({Node, NameType, Cookie} = _NetConfig) ->
  case net_kernel:start([Node, NameType]) of
    {ok, _Pid} -> set_cookie(Cookie);
    {error, {already_started, _Pid}} -> set_cookie(Cookie);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Stop Erlang's networking.

-spec stop_network() ->
  ok | {error, term()}.

stop_network() ->
  case net_kernel:stop() of
    ok -> ok;
    {error, not_found} -> ok;
    {error, Reason} -> {error, Reason}
  end.

%% @doc Set magic cookie, if one was provided.

-spec set_cookie(atom() | none) ->
  ok.

set_cookie(none = _Cookie) ->
  ok;
set_cookie(Cookie) ->
  erlang:set_cookie(node(), Cookie),
  ok.

%% @doc Read network configuration and initial state from app environment.

-spec get_network_config() ->
  {started, net_config()} | {stopped, net_config() | undefined}.

get_network_config() ->
  case application:get_env(net) of
    {ok, {Node, NameType, Cookie}} ->
      NetInitialStatus = case application:get_env(net_start) of
        {ok, true}  -> started;
        {ok, false} -> stopped
      end,
      {NetInitialStatus, {Node, NameType, Cookie}};
    undefined ->
      {stopped, undefined}
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
