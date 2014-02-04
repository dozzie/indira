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

set_environment([]) ->
  ok;
set_environment([{App, Opt, Value} | Options]) ->
  set_option(App, Opt, Value),
  set_environment(Options).

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

sleep_forever() ->
  receive
    % ignore all the messages (none should arrive, anyway)
    _Any -> sleep_forever()
  end.

%% }}}
%%----------------------------------------------------------
%% start application (recursively) {{{

%% NOTE: this function is intended for being run from escript, not for
%% introducing dependency in convenience wrappers in application's code

start_rec(App) ->
  % defaults to the same as application:start()
  start_rec(App, temporary).

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

%% TODO:
%%  * compile + load
%%  * load plugins from multiple directories
%%  * reload plugins from multiple directories
%%    hint:
%%    [T || {_,F} = T <- code:all_loaded(), is_list(F), lists:prefix(P,F)]
%%    code:purge(), code:soft_purge()
%%  * problem with reload: somebody wants to run something between purge and
%%    load (race condition)

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

write_pidfile(undefined) ->
  ok;
write_pidfile(_Filename) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% setup logging {{{

%% * redirect standard_io (group leader)
%% * redirect standard_error
%% * disable TTY logging
%% * set logfile
%% * configure and start lager
setup_logging(_Options) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% distributed Erlang {{{

distributed(Name) ->
  distributed(Name, longnames).

distributed(Name, NameType) ->
  % NameType :: shortnames | longnames
  net_kernel:start([Name, NameType]).

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

%% send command to Indira process
command(Indira, Line) ->
  gen_server:call(Indira, {command, Line}).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
