%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   {@link gen_indira_sock_stream} top-level supervisor.
%%%
%%% @TODO Read part of the restart strategy from listen module, if applicable.
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_sock_stream_sup).

-behaviour(supervisor).

-export([start_link/3]).

%% workers supervisor
-export([start_connection_supervisor/1]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% starting supervisor process

%% @doc Start the supervisor process.
%%
%%   `ConnHandler' is expected to have function name explicit.
%%
%% @spec start_link(gen_indira_sock_stream:listener_module(),
%%                  gen_indira_sock_stream:connection_handler(),
%%                  gen_indira_sock_stream:listen_address()) ->
%%   {ok, Pid} | {error, Reason}

start_link(ListenModule, ConnHandler, Address) ->
  supervisor:start_link(?MODULE, {ListenModule, ConnHandler, Address}).

%%----------------------------------------------------------
%% wrappers around `supervisor' module

%% @doc Start connection handlers' supervisor.
%%
%% @see gen_indira_sock_stream_connection_sup
%%
%% @spec start_connection_supervisor(pid()) ->
%%   {ok, Pid} | {error, Reason}

start_connection_supervisor(Supervisor) ->
  % FIXME: this is subject to a race condition with parent
  % NOTE: this is somewhat ugly to manually search through the children, but
  % I have little better alternatives on how this should work actually
  Children = supervisor:which_children(Supervisor),
  {_, Pid, _, _} = lists:keyfind(connection_sup, 1, Children),
  {ok, Pid}.

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize supervisor.

init({ListenModule, ConnHandler, Address} = _Args) ->
  ListenerArgs = [self(), ListenModule, ConnHandler, Address],

  Strategy = {one_for_all, 5, 10},
  Children = [
    {listener,
      {gen_indira_sock_stream, start_link_supervised, ListenerArgs},
      % FIXME: dynamic? ListenModule?
      permanent, 5000, worker, [gen_indira_sock_stream]},
    {connection_sup,
      {gen_indira_sock_stream_connection_sup, start_link, [ConnHandler]},
      permanent, 5000, supervisor, [gen_indira_sock_stream_connection_sup]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
