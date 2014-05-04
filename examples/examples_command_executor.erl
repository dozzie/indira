%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example command executor.
%%%
%%% ```
%%% -module(command_executor).
%%%
%%% -behaviour(gen_server).
%%%
%%% %% supervision tree API
%%% -export([start_link/0]).
%%%
%%% %% gen_server callbacks
%%% -export([init/1, terminate/2]).
%%% -export([handle_call/3, handle_cast/2, handle_info/2]).
%%% -export([code_change/3]).
%%%
%%% %%%---------------------------------------------------------------------
%%%
%%% -record(state, {}).
%%%
%%% %%%---------------------------------------------------------------------
%%% %%% supervision tree API
%%% %%%---------------------------------------------------------------------
%%%
%%% start_link() ->
%%%   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%%
%%% %%%---------------------------------------------------------------------
%%% %%% gen_server callbacks
%%% %%%---------------------------------------------------------------------
%%%
%%% init(_Args) ->
%%%   {ok, #state{}}.
%%%
%%% terminate(_Reason, _State) ->
%%%   ok.
%%%
%%% handle_call(_Request, _From, State) ->
%%%   {reply, {error, unsupported}, State}.
%%%
%%% handle_cast(_Request, State) ->
%%%   {noreply, State}.
%%%
%%% handle_info({command, ReplyTo, ChannelID, Command} = _Message, State) ->
%%%   % XXX: this is where command execution or dispatching should take place
%%%   io:fwrite("got command ~p~n", [Command]),
%%%   Reply = [{error, <<"unsupported command">>}],
%%%   ReplyTo ! {result, ChannelID, Reply},
%%%   {noreply, State};
%%%
%%% handle_info(_Message, State) ->
%%%   {noreply, State}.
%%%
%%% code_change(_OldVsn, State, _Extra) ->
%%%   {ok, State}.
%%% '''
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(examples_command_executor).

%%% Module for documentation only.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
