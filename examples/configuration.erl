%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira configuration options.
%%%
%%%   Indira uses following configuration keys ({@link application:get_env/2}):
%%%
%%%   <ul>
%%%     <li>
%%%       <i>listen</i> ({@type [@{Module :: module(), Args :: term()@}]}) --
%%%       a list of administrative sockets addresses, where `Module'
%%%       implements {@link gen_indira_socket} and `Args' describes socket's
%%%       address; see {@link indira_unix}, {@link indira_tcp}, and
%%%       {@link indira_udp}
%%%     </li>
%%%     <li>
%%%       <i>command</i> ({@type @{Module :: module(), Args :: term()@} |
%%%       fun() | @{fun(), term()@}}) -- command handler module, where
%%%       `Module' implements {@link gen_indira_command} behaviour and `Args'
%%%       is its additional configuration, if one is necessary; it can also be
%%%       a one-argument function or a pair of a two-argument function and
%%%       a term (`{fun(), Arg}'), but this is not recommended
%%%     </li>
%%%     <li>
%%%       <i>reload_function</i> ({@type @{Mod :: atom(), Fun :: atom(),
%%%       Args :: [term()]@}}) -- function to be called by
%%%       {@link indira:reload/0}
%%%     </li>
%%%     <li>
%%%       <i>pidfile</i> ({@type file:filename()}) -- path to a pidfile to
%%%       write at start and delete on shutdown (may be left unset)
%%%     </li>
%%%     <li>
%%%       <i>workdir</i> ({@type file:filename()}) -- path that should be
%%%       the working directory of BEAM machine process
%%%     </li>
%%%     <li>
%%%       <i>net</i> ({@type @{Node :: atom(), NameType :: shortnames |
%%%       longnames, Cookie :: atom() | @{file, file:filename()@} | none@}})
%%%       -- distributed Erlang network configuration
%%%     </li>
%%%     <li>
%%%       <i>net_start</i> ({@type boolean()}; default: `false') -- whether to
%%%       configure Erlang networking according to <i>net</i> setting at
%%%       Indira's start or delay it; networking can be started and stopped
%%%       using {@link indira:distributed_start/0} and
%%%       {@link indira:distributed_stop/0}
%%%     </li>
%%%     <li>
%%%       <i>start_before</i> ({@type [App :: atom()]}) -- list of
%%%       applications to be started before Indira
%%%     </li>
%%%     <li>
%%%       <i>start_after</i> ({@type [App :: atom()]}) -- list of
%%%       applications to be started after Indira, but before daemonized
%%%       application
%%%     </li>
%%%   </ul>
%%%
%%%   == How to setup Indira ==
%%%
%%%   === Setup with command line flags ===
%%%
%%%   Indira may be configured directly in command line:
%%%
%```
%$ erl \
%    -indira listen '[{indira_tcp, {"localhost", 16667}}]' \
%    -indira command '{example_command_handler, []}' \
%    -s indira \
%    other args ...
%'''
%%%
%%%   This way no additional configuration file is needed and very little code
%%%   needs to be written.
%%%
%%%   This migt be useful trick for injecting Indira to already written
%%%   services, e.g. to provide additional instrumentation. Note that
%%%   `example_command_handler' doesn't even need to come from original
%%%   service -- it could be a custom module. Consult
%%%   {@link gen_indira_command} to see how to write one.
%%%
%%%   === Setup with `-config myapp.config' ===
%%%
%%%   Instead of specifying flags one may provide configuration file with
%%%   `-config myapp.config' option. This is how it could look like:
%%%
%```
%[
%  {some_app, [
%    % some_app's environment
%  ]},
%  {indira, [
%    {listen, [
%      {indira_tcp, {"localhost", 16667}},
%      {indira_unix, "/var/run/example_app/control"}
%    ]},
%    {command, {example_command_handler, []}}
%  ]},
%  % ...
%].
%'''
%%%
%%%   === Setup with Erlang code ===
%%%
%%%   ==== Indira API ====
%%%
%```
%#!/usr/bin/escript
%
%main(_Args) ->
%  indira:indira_setup([
%    {listen, [
%      {indira_tcp, {"localhost", 16667}}
%    ]},
%    {command, fun(C) -> handle_command_fun(C) end}
%  ]),
%  % note that this starts Indira as the only application; you may want to
%  % use `indira:daemonize(SomeApp, [])' instead of these two
%  indira:start_rec(indira),
%  indira:sleep_forever().
%
% % simple and dumb command handler
% % note that this will be passed as a fun, not as a module handler;
% % see `gen_indira_command' docs
%handle_command_fun(<<"stop">> = _Command) ->
%  init:stop(),
%  ok;
%handle_command_fun(Command) ->
%  io:fwrite("got command ~p~n", [Command]),
%  unsupported.
%'''
%%%
%%%   ==== application:set_env/3 ====
%%%
%```
%#!/usr/bin/escript
%
%main(_Args) ->
%  % this is an alternative to indira:indira_setup/1 and indira:set_env/2
%  % note that application needs to be loaded for application:set_env/3
%  application:load(indira),
%  application:set_env(indira, listen, [{indira_tcp, {"localhost", 16667}}]),
%  application:set_env(indira, command, fun(C) -> handle_command_fun(C) end),
%  indira:start_rec(indira),
%  indira:sleep_forever().
%...
%'''
%%%
%%% @see gen_indira_command
%%% @see gen_indira_cli
%%% @see indira_tcp
%%% @see indira_udp
%%% @see indira_unix
%%% @end
%%%---------------------------------------------------------------------------

-module(configuration).

%%% Module for documentation only.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
