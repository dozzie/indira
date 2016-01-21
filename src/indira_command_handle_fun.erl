%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Command handler that calls specified function.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_command_handle_fun).

-behaviour(gen_indira_command).

%% Indira command handler API
-export([handle_command/2]).

%%%---------------------------------------------------------------------------
%%% Indira command handler API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Handle commands.

handle_command(Command, {Function}) ->
  Function(Command);

handle_command(Command, {Function, Arg}) ->
  Function(Command, Arg).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
