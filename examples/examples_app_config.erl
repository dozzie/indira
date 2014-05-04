%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example application configuration.
%%%
%%% == Setup with command line flags ==
%%%
%%%   Indira may be configured directly in command line:
%%%
%%% ```
%%% $ erl \
%%%     -indira listen '[{indira_tcp, {any,5500}}]' \
%%%     -indira commander example_command_executor \
%%%     -s indira \
%%%     other args ...
%%% '''
%%%
%%%   This way no additional configuration file is needed and very little code
%%%   needs to be written.
%%%
%%%   This migt be useful trick for injecting Indira to already written
%%%   services, e.g. to provide additional instrumentation. Note that
%%%   `example_command_executor' doesn't even need to come from original
%%%   service -- it could be custom module.
%%%
%%% == Setup with `-config myapp.config' ==
%%%
%%%   Instead of specifying flags one may provide configuration file with
%%%   `-config myapp.config' option. This is how it could look like:
%%%
%%% ```
%%% [
%%%   {some_app, [
%%%     % some_app's environment
%%%   ]},
%%%   {indira, [
%%%     {listen, [
%%%       {indira_tcp, {"localhost", 16667}},
%%%       {indira_unix, "/var/run/my_app.sock"}
%%%     ]},
%%%     % there should be process registered as `my_app_command'
%%%     {commander, my_app_command}
%%%   ]},
%%%   % ...
%%% ].
%%% '''
%%%
%%% == Setup with Erlang code ==
%%%
%%% === Indira API ===
%%%
%%% ```
%%% #!/usr/bin/escript
%%%
%%% main(_Args) ->
%%%   Pid = spawn(fun() -> command_executor() end),
%%%   % these two calls are enough to configure Indira in Erlang code
%%%   indira:set_option(indira, listen, [{indira_tcp, {any, 5500}}]),
%%%   indira:set_option(indira, commander, Pid),
%%%   indira:setup_logging(my_app, [stdout]),
%%%   indira:start_rec(indira),
%%%   indira:sleep_forever().
%%%
%%% % simple and dumb command executor
%%% command_executor() ->
%%%   receive
%%%     {command, ReplyTo, ChannelID, Command} ->
%%%       io:fwrite("got command ~p~n", [Command]),
%%%       ReplyTo ! {result, ChannelID, unsupported};
%%%     _Any ->
%%%       ignore
%%%   end,
%%%   command_executor().
%%% '''
%%%
%%% === application:set_env/3 ===
%%%
%%% ```
%%% #!/usr/bin/escript
%%%
%%% main(_Args) ->
%%%   Pid = spawn(fun() -> command_executor() end),
%%%   % this is an alternative to indira:set_option/3
%%%   % note that application needs to be loaded for application:set_env/3
%%%   application:load(indira),
%%%   application:set_env(indira, listen, [{indira_tcp, {any, 5500}}]),
%%%   application:set_env(indira, commander, Pid),
%%%   % rest is the same as in the example above
%%%   indira:setup_logging(my_app, [stdout]),
%%%   indira:start_rec(indira),
%%%   indira:sleep_forever().
%%% % ...
%%% '''
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(examples_app_config).

%%% Module for documentation only.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
