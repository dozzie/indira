%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira main API.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira).

%% API for escript
-export([set_environment/1, set_option/3]).
-export([sleep_forever/0]).
-export([start_rec/1, start_rec/2]).
-export([load_plugins_dir/1]).
-export([write_pidfile/1]).
-export([setup_logging/1]).
-export([distributed/1]).
-export([distributed/2]).
-export([distributed/3]).

%% API for listeners
-export([command/2]).

%%%---------------------------------------------------------------------------
%%% API for escript
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% set application configuration parameters (environment) {{{

%% @doc Set list of configuration options for appropriate applications.
set_environment([]) ->
  ok;
set_environment([{App, Opt, Value} | Options]) ->
  set_option(App, Opt, Value),
  set_environment(Options).

%% @doc Set configuration option for specified application.
set_option(App, Option, Value) ->
  case application:load(App) of
    ok -> ok;
    {error, {already_loaded, App}} -> ok;
    {error, Reason} -> erlang:error(Reason)
  end,
  application:set_env(App, Option, Value).

%% }}}
%%----------------------------------------------------------
%% sleep forever {{{

%% @doc Sleep forever. Function intended for use in `main()' function in
%%   `escript' code.
sleep_forever() ->
  receive
    % ignore all the messages (none should arrive, anyway)
    _Any -> sleep_forever()
  end.

%% }}}
%%----------------------------------------------------------
%% start application (recursively) {{{

%% @doc Start application along with all its dependencies.
%%   This function is intended for being run from `escript' code.
%%
%%   <b>NOTE</b>: Application start type is `permanent'.
%% @see application:start/2
start_rec(App) ->
  % defaults to the same as application:start()
  start_rec(App, permanent).

%% @doc Start application along with all its dependencies.
%%   This function is intended for being run from `escript' code.
%% @see application:start/2
start_rec(App, StartType) ->
  case application:start(App, StartType) of
    {error, {not_started, App1}} ->
      ok = start_rec(App1, StartType),
      start_rec(App);
    {error, {already_started, App}} ->
      ok;
    {error, _Any} = Error ->
      Error;
    ok ->
      ok
  end.

%% }}}
%%----------------------------------------------------------
%% load plugins (BEAM files) from directory {{{

%% @TODO Compile + load function.
%% @TODO Load plugins from multiple directories.
%% @TODO Reload plugins from multiple directories.
%%   Hint:
%%   ```
%%   [T || {_,F} = T <- code:all_loaded(), is_list(F), lists:prefix(P,F)]
%%   '''
%%   {@link code:purge/1}, {@link code:soft_purge/1}
%% @TODO Problem with reload: somebody wants to run something between purge
%%   and load (race condition).

%% @doc Load <tt>*.beam</tt> files from specified directory.
load_plugins_dir(Directory) ->
  Files = filelib:fold_files(
    Directory, "\\.beam$", false,
    fun(F, Acc) ->
      AF = filename:absname(F),
      CF = string:substr(AF, 1, length(AF) - 5),
      {module, Mod} = code:load_abs(CF),
      [{Mod, AF} | Acc]
    end, []
  ),
  Files.

%% }}}
%%----------------------------------------------------------
%% write pidfile {{{

%% @doc Write PID file.
write_pidfile(undefined) ->
  ok;
write_pidfile(_Filename) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% setup logging {{{

%% @TODO redirect `standard_io' (group leader)
%% @TODO redirect `standard_error'
%% @TODO disable TTY logging
%% @TODO set logfile
%% @TODO configure and start lager

%% @doc Setup logging.
setup_logging(_Options) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% distributed Erlang {{{

%% @doc Configure Erlang networking (distributed Erlang).
distributed(Name) ->
  distributed(Name, longnames).

%% @doc Configure Erlang networking (distributed Erlang).
distributed(Name, NameType) ->
  % NameType :: shortnames | longnames
  net_kernel:start([Name, NameType]).

%% @doc Configure Erlang networking (distributed Erlang).
distributed(Name, NameType, Cookie) ->
  net_kernel:start([Name, NameType]),
  CookieAtom = case Cookie of
    _ when is_atom(Cookie)   -> Cookie;
    _ when is_list(Cookie)   -> list_to_atom(Cookie);
    _ when is_binary(Cookie) -> binary_to_atom(Cookie, utf8)
  end,
  erlang:set_cookie(node(), CookieAtom).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% API for listeners
%%%---------------------------------------------------------------------------

%% @doc Send command to Indira router.
command(Indira, Line) ->
  gen_server:call(Indira, {command, Line}).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
