%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example application configuration.
%%%
%%% == Setup with `-config myapp.config' ==
%%%
%%% ```
%%% % myapp.config
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
