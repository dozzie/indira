%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Options processing functions.
%%%
%%%   This module contains functions for parsing command line options and for
%%%   populating application environment ({@link application:set_env/3}) with
%%%   values read from config files and other sources.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_opts).

%% string processing
-export([strsplit/2]).
%% command line arguments parsing
-export([foldg/3, folds/3]).
%% propagating config over application environment
-export([set_env/4]).

%%%---------------------------------------------------------------------------
%%% data types {{{

%%----------------------------------------------------------

-type argument() :: string().
%% Command line argument.

-type accumulator() :: term().
%% Fold function accumulator.

-type foldg_fun_simple() ::
  fun(([argument(), ...], accumulator()) ->
        accumulator() | {take, pos_integer(), accumulator()} | {error, term()}).
%% Fold function for non-splitting general fold.

-type foldg_fun_split() ::
  fun((single | split, [argument(), ...], accumulator()) ->
        accumulator() | {take, pos_integer(), accumulator()} | {error, term()}).
%% Fold function for `--foo=bar' splitting general fold.

-type folds_fun() ::
  fun((argument() | [argument(), ...], accumulator()) ->
        accumulator() | {need, pos_integer()} | {error, term()}).
%% Fold function for simple fold.

%%----------------------------------------------------------

-type config() :: term().
%% Structure with configuration options loaded from a file.

-type config_key() :: term().
%% Option name to lookup in {@type config()}.

-type config_value() :: term().
%% Value of {@type config_key()} in {@type config()}.

-type env_key() :: {AppName :: atom(), Parameter :: atom()}.
%% Application environment parameter name.

-type set_spec() ::
    {config_key(), env_key()}
  | {config_key(), env_key(), [set_option()]}.
%% Specification for setting an app parameter.

-type set_option() :: append | if_set | if_unset.
%% When and how to set an app parameter.

%%----------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% command line arguments parsing
%%%---------------------------------------------------------------------------

%% @doc General fold over command line arguments.
%%
%%   Function passed as an argument is called with the <em>list</em> of
%%   remaining (unprocessed) command line arguments.
%%
%%   The passed function decides how many arguments to consume by
%%   returning tuple `{take, N, NewAcc}', so the next call will be with `N'
%%   leading elements skipped.
%%
%%   If `Fun' returns `{error, Reason}', whole iteration is terminated and
%%   `{error, {Arg, Reason}}' is returned to the caller, with `Arg' being the
%%   argument at which the error occurred.
%%
%%   Anything else than `{take, N, _}' or `{error, _}' is considered to be
%%   a new accumulator. The same can be achieved by returning `{take, 1, _}'.
%%
%%   If `Fun' is two-argument function, simple iteration over arguments list
%%   takes place.
%%
%%   If `Fun' is three-argument, each `"--foo=XXX"' argument is
%%   split on the first `"="' character and the right side of the split
%%   (`"XXX"') is inserted into arguments list just after the raw option
%%   `"--foo"', so it becames `["--foo", "XXX", RestArgs ...]'. To notify that
%%   this operation was done, `Fun(split, ArgList, Acc)' is called. Obviously,
%%   if split was <em>not done</em>, `Fun(simple, ArgList, Acc)' is called.
%%
%%   If `Fun(split, _, Acc)' doesn't return `{take, N, NewAcc}', it results in
%%   an error (`{error, {Arg, excessive_value}}'). However, `Fun' can decide
%%   it is OK to only consume option and leave its argument in the argument
%%   list by returning `{take, 1, _}'.

-spec foldg(foldg_fun_simple() | foldg_fun_split(), accumulator(),
            [argument()]) ->
  {ok, accumulator()} | {error, {argument(), FoldError | term()}}
  when FoldError :: excessive_value.

foldg(Fun, Acc, ArgList) when is_function(Fun, 2) ->
  foldg_simple(Fun, Acc, ArgList);
foldg(Fun, Acc, ArgList) when is_function(Fun, 3) ->
  foldg_split(Fun, Acc, ArgList).

%%----------------------------------------------------------
%% foldg_simple() {{{

%% @doc Simple general fold over options.
%%   `--foo' options are not subject to split on `"="' character.

foldg_simple(_Fun, Acc, [] = _ArgList) ->
  {ok, Acc};
foldg_simple(Fun, Acc, [Arg | RestArgs] = ArgList) ->
  case Fun(ArgList, Acc) of
    % user consumed N arguments of the list
    {take, N, NewAcc} when is_integer(N), N > 0 ->
      foldg_simple(Fun, NewAcc, lists:nthtail(N, ArgList));
    {error, Reason} ->
      {error, {Arg, Reason}};
    % everything else is a new accumulator
    NewAcc ->
      foldg_simple(Fun, NewAcc, RestArgs)
  end.

%% }}}
%%----------------------------------------------------------
%% foldg_split() {{{

%% @doc Splitting general fold over options.
%%   `--foo' options are subject to split on `"="' character, and the function
%%   is passed an argument to indicate whether split took place for this
%%   option or not.

foldg_split(_Fun, Acc, [] = _ArgList) ->
  {ok, Acc};
foldg_split(Fun, Acc, ["--" ++ _ = Arg | RestArgs] = ArgList) ->
  % split on "=" only the arguments looking like `--foo...'
  case strsplit(Arg, $=) of
    [Arg]        -> foldg_split_no_value(Fun, Acc, ArgList);
    [ArgN, ArgV] -> foldg_split_value(Fun, Acc, [ArgN, ArgV | RestArgs])
  end;
foldg_split(Fun, Acc, [_ | _] = ArgList) ->
  % if it doesn't look like an option, don't split it on "="
  foldg_split_no_value(Fun, Acc, ArgList).

%% @doc Splitting general fold, worker for when no split was done.

foldg_split_no_value(Fun, Acc, [Arg | RestArgs] = ArgList) ->
  % at the head of `ArgList' is either a simple `--foo' option or a non-option
  case Fun(single, ArgList, Acc) of
    % user consumed N arguments of the list
    {take, N, NewAcc} when is_integer(N), N > 0 ->
      foldg_split(Fun, NewAcc, lists:nthtail(N, ArgList));
    {error, Reason} ->
      {error, {Arg, Reason}};
    NewAcc ->
      foldg_split(Fun, NewAcc, RestArgs)
  end.

%% @doc Splitting general fold, worker for when split actually took place.

foldg_split_value(Fun, Acc, [Arg | _] = ArgList) ->
  % at the head of `ArgList' was a `--foo=bar' option and it was split into
  % two elements
  case Fun(split, ArgList, Acc) of
    % user consumed N arguments of the list
    % even if it was 1, user clearly said it was OK to use the value to this
    % option as a possibly new option
    {take, N, NewAcc} when is_integer(N), N > 0 ->
      foldg_split(Fun, NewAcc, lists:nthtail(N, ArgList));
    {error, Reason} ->
      {error, {Arg, Reason}};
    % it is an error to only consume one argument, when in fact it was just
    % a half of the original argument
    _NewAcc ->
      {error, {Arg, excessive_value}}
  end.

%% }}}
%%----------------------------------------------------------

%% @doc Simple fold over command line arguments.
%%
%%   Function `Fun' is called with only a single command line argument. If the
%%   function returns `{need, N}', it will be immediately called with a list
%%   of `N+1' arguments (current and the next `N'), (unless the list of
%%   remaining arguments is shorter than `N', in which case the whole
%%   iteration is terminated with `{error, {Arg, not_enough_args}}' result).
%%
%%   As with {@link foldg/3}, `Fun' returning `{error, Reason}' terminates the
%%   iteration with result of `{error, {Arg, Reason}}' (on the call after
%%   `{need, N}', only the first element of the list is used).

-spec folds(folds_fun(), accumulator(), [argument()]) ->
  {ok, accumulator()} | {error, {argument(), FoldError | term()}}
  when FoldError :: not_enough_args.

folds(_Fun, Acc, [] = _ArgList) ->
  {ok, Acc};
folds(Fun, Acc, [Arg | RestArgs] = _ArgList) ->
  case Fun(Arg, Acc) of
    {need, N} when is_integer(N), N > 0 ->
      folds_second_call(Fun, Acc, N, Arg, RestArgs);
    {error, Reason} ->
      {error, {Arg, Reason}};
    % everything else is a new accumulator
    NewAcc ->
      folds(Fun, NewAcc, RestArgs)
  end.

%%----------------------------------------------------------
%% folds_second_call() {{{

%% @doc Call {@link folds/3}-supplied function with requested args list.
%%   To use when function requested `N' arguments to an option.

folds_second_call(Fun, Acc, N, Opt, ArgList) ->
  case listsplit(ArgList, N) of
    {OptArgs, RestArgs} ->
      case Fun([Opt | OptArgs], Acc) of
        {error, Reason} -> {error, {Opt, Reason}};
        NewAcc -> folds(Fun, NewAcc, RestArgs)
      end;
    error ->
      {error, {Opt, not_enough_args}}
  end.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% propagating config over application environment
%%%---------------------------------------------------------------------------

%% @doc Set application environment from config according to specification.
%%   Function pre-loads all the (potentially) necessary applications.
%%
%%   If validation function (called as `Validate(Key, EnvKey, Value)') returns
%%   `{error, Reason}', it will be reported as
%%   `{error, {Key, EnvKey, Reason}}'.

-spec set_env(ConfigGet, Validate, config(), [set_spec()]) ->
  ok | {error, {config_key(), env_key(), term()} | term()}
  when
    ConfigGet :: fun((config_key(), config()) ->
                       config_value()),
    Validate  :: fun((config_key(), env_key(), config_value()) ->
                       ok | {ok, NewValue :: term()} |
                       ignore | {error, term()}).

set_env(ConfigGet, Validate, Config, SetSpecs) ->
  case load_apps(SetSpecs) of
    ok -> set_env_loop(ConfigGet, Validate, Config, SetSpecs);
    {error, Reason} -> {error, Reason}
  end.

%%----------------------------------------------------------
%% set_env_loop() {{{

%% @doc Set app environment, worker for {@link set_env/4}.

set_env_loop(_ConfigGet, _Validate, _Config, [] = _SetSpecs) ->
  ok;
set_env_loop(ConfigGet, Validate, Config, [Spec | RestSpecs] = _SetSpecs) ->
  case Spec of
    {Key, EnvKey, SetOpts} -> ok;
    {Key, EnvKey} -> SetOpts = []
  end,
  Value = ConfigGet(Key, Config),
  case Validate(Key, EnvKey, Value) of
    ignore ->
      set_env_loop(ConfigGet, Validate, Config, RestSpecs);
    ok ->
      set_app_env(EnvKey, Value, SetOpts),
      set_env_loop(ConfigGet, Validate, Config, RestSpecs);
    {ok, EnvValue} ->
      set_app_env(EnvKey, EnvValue, SetOpts),
      set_env_loop(ConfigGet, Validate, Config, RestSpecs);
    {error, Reason} ->
      {error, {Key, EnvKey, Reason}}
  end.

%% }}}
%%----------------------------------------------------------
%% set_app_env() {{{

%% @doc Set an application parameter to specified value.

-spec set_app_env(env_key(), term(), [set_option()]) ->
  ok.

set_app_env({App, Param} = _EnvKey, Value, Options) ->
  case should_set(App, Param, Options) of
    true ->
      case should_append(Options) of
        true ->
          CurrentValue = case application:get_env(App, Param) of
            {ok, V} -> V;
            undefined -> []
          end,
          application:set_env(App, Param, CurrentValue ++ [Value]);
        false ->
          application:set_env(App, Param, Value)
      end;
    false ->
      ok
  end.

%% @doc Determine whether an option should be set or not, according to
%%   options.

-spec should_set(atom(), atom(), [set_option()]) ->
  boolean().

should_set(App, Param, Options) ->
  Set = case application:get_env(App, Param) of
    {ok, _} -> set;
    undefined -> unset
  end,
  IfSet   = proplists:get_bool(if_set, Options),
  IfUnset = proplists:get_bool(if_unset, Options),
  case {IfSet, IfUnset, Set} of
    {true, _, set}    -> true;
    {_, true, unset}  -> true;
    {false, false, _} -> true; % if neither specified, set always
    {_, _, _}         -> false
  end.

%% @doc Determine whether an option should be appended to a current value.

-spec should_append([set_option()]) ->
  boolean().

should_append(Options) ->
  proplists:get_bool(append, Options).

%% }}}
%%----------------------------------------------------------
%% load_apps() {{{

%% @doc Load applications that will be potentially used in `SetSpecs'.

-spec load_apps([set_spec()]) ->
  ok | {error, term()}.

load_apps([] = _SetSpecs) ->
  ok;
load_apps([{_Key, {App, _Par}} | RestSpecs] = _SetSpecs) ->
  case application:load(App) of
    ok -> load_apps(RestSpecs);
    {error, {already_loaded, App}} -> load_apps(RestSpecs);
    {error, Reason} -> {error, Reason}
  end;
load_apps([{_Key, {App, _Par}, _Opts} | RestSpecs] = _SetSpecs) ->
  case application:load(App) of
    ok -> load_apps(RestSpecs);
    {error, {already_loaded, App}} -> load_apps(RestSpecs);
    {error, Reason} -> {error, Reason}
  end;
load_apps(_SetSpecs) ->
  {error, badarg}.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% string helpers
%%%---------------------------------------------------------------------------

%% @doc Split string into two parts on a single character.
%%
%%   The returned list is either one- or two-element. Split is done on the
%%   first occurrence of `SplitChar'.

-spec strsplit(string(), char()) ->
  [string(), ...].

strsplit(String, SplitChar) ->
  case lists:splitwith(fun(C) -> C =/= SplitChar end, String) of
    {String, ""} -> [String];
    {Left, [SplitChar | Right]} -> [Left, Right]
  end.

%% @doc Split a list into two lists, first of length `N'.

-spec listsplit(list(), pos_integer()) ->
  {list(), list()} | error.

listsplit(List, N) ->
  try
    lists:split(N, List)
  catch
    error:badarg -> error
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
