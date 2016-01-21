%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Indira command router process.
%%%
%%%   This process receives all the lines from {@link gen_indira_listener}
%%%   processes and spawns {@link indira_command} workers to parse the
%%%   commands, execute them, serialize their replies, and send them back to
%%%   listeners.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_commander).

-behaviour(gen_server).

%% public interface
-export([command/1, command/2]).

%% supervision tree API
-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {}).

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Send command to Indira router.
%%   The process calling this function will get the response as a message.

-spec command(indira_json:json_string()) ->
  ok.

command(Line) ->
  gen_server:call(?MODULE, {command, self(), Line}).

%% @doc Send command to Indira router.
%%   The process calling this function will get the response as a message.
%%
%%   `RoutingKey' is an additional information to tell apart between multiple
%%   clients and will be included in command reply message.
%%
%%   This call form is only needed when a single process handles multiple
%%   clients, like {@link indira_udp}.

-spec command(term(), indira_json:json_string()) ->
  ok.

command(RoutingKey, Line) ->
  gen_server:call(?MODULE, {command, {self(), RoutingKey}, Line}).

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start commander process.

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @private
%% @doc Start commander process.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize event handler.

init(_Args) ->
  State = #state{},
  {ok, State}.

%% @private
%% @doc Clean up after event handler.

terminate(_Arg, _State) ->
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

handle_call({command, ReplyTo, Line} = _Request, _From, State) ->
  % should be `{ok, Pid}', but don't care at this point
  indira_command_sup:spawn_command(Line, ReplyTo),
  {reply, ok, State};

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
%%% vim:ft=erlang:foldmethod=marker
