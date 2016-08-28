%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Process for writing and deleting pidfile.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_pidfile).

-behaviour(gen_server).

%% supervision tree API
-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {
  pidfile :: file:filename()
}).

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start pidfile manager process.

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @private
%% @doc Start pidfile manager process.

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
  case application:get_env(pidfile) of
    {ok, Filename} ->
      AbsName = filename:absname(Filename),
      ok = file:write_file(AbsName, [os:getpid(), $\n]),
      % necessary to trap exit to have terminate() called on shutdown (see
      % gen_server documentation)
      process_flag(trap_exit, true),
      State = #state{pidfile = AbsName};
    undefined ->
      State = #state{}
  end,
  {ok, State}.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Arg, _State = #state{pidfile = undefined}) ->
  ok;
terminate(_Arg, _State = #state{pidfile = Filename}) ->
  file:delete(Filename),
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

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
