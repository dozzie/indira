%%%---------------------------------------------------------------------------
%%% @doc
%%%   Functions to work with command line.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_cli).

-export([execute/3]).
-export([folds/3, foldg/3]).
-export([send_one_command/4]).

%%%---------------------------------------------------------------------------
%%% command line execution
%%%---------------------------------------------------------------------------

%% @doc Execute an operation specified in command line using {@link
%%   gen_indira_cli}.
%%
%% Error formats:
%% <ul>
%%   <li>`{error, {arguments, Reason}' when `parse_arguments()' callback
%%       returns an error</li>
%%   <li>`{error, {format, Reason}}' when `format_request()' callback returns
%%       an error</li>
%%   <li>`{error, {send, Reason}}' on connection or (de)serialization error
%%       (the same as for {@link send_one_command/4})</li>
%%   <li>`{error, {bad_return_value, Value}}' when any of the callbacks
%%       returns an invalid value</li>
%%   <li>`{error, Reason}' when `handle_command()' or `handle_reply()',
%%       whichever is called, return an error</li>
%% </ul>

-spec execute([string()], module(), term()) ->
  ok | help | {error, Reason}
  when Reason :: {arguments, term()}
               | {format, term()}
               | {send, bad_request_format | bad_reply_format | term()}
               | {bad_return_value, term()}
               | term().

execute(ArgList, CLIHandler, Defaults) ->
  case CLIHandler:parse_arguments(ArgList, Defaults) of
    {ok, Command, Options} ->
      execute_command(CLIHandler, Command, Options);
    {send, {_SockMod, _SockAddr} = Address, Command, Options} ->
      send_command(CLIHandler, Command, Options, Address);
    help ->
      help;
    {error, Reason} ->
      {error, {arguments, Reason}};
    Result ->
      {error, {bad_return_value, Result}}
  end.

%%----------------------------------------------------------
%% execute actions returned by parse_arguments() {{{

%% @doc Pass a complex command to CLI module for execution.

-spec execute_command(module(), gen_indira_cli:command(),
                      gen_indira_cli:options()) ->
  ok | {error, term()}.

execute_command(CLIHandler, Command, Options) ->
  case CLIHandler:handle_command(Command, Options) of
    ok -> ok;
    {error, Reason} -> {error, Reason};
    Result -> {error, {bad_return_value, Result}}
  end.

%% @doc Send a simple command to daemon and pass the reply to CLI module.

-spec send_command(module(), gen_indira_cli:command(),
                   gen_indira_cli:options(), gen_indira_cli:socket_address()) ->
  ok | {error, Reason}
  when Reason :: {send, bad_request_format | bad_reply_format | term()}
               | {format, term()}
               | {bad_return_value, term()}
               | term().

send_command(CLIHandler, Command, Options, {SockMod, SockAddr} = _Address) ->
  case CLIHandler:format_request(Command, Options) of
    {ok, Request} ->
      case send_one_command(SockMod, SockAddr, Request, []) of
        {ok, Reply} ->
          case CLIHandler:handle_reply(Reply, Command, Options) of
            ok -> ok;
            {error, Reason} -> {error, Reason};
            Result -> {error, {bad_return_value, Result}}
          end;
        {error, Reason} ->
          {error, {send, Reason}}
      end;
    {error, Reason} ->
      {error, {format, Reason}};
    Result ->
      {error, {bad_return_value, Result}}
  end.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% sending a command through admin socket (from command line script)
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% send one command, timeout {{{

%% @doc Open a connection to daemon's Indira, send a command, receive a reply.
%%
%%   `Module' implements {@link gen_indira_socket} behaviour.
%%
%%   If `retry' option was specified, refused connection will result in
%%   retrying to connect indefinitely (or until timeout occurs).
%%
%%   NOTE: received hashes are compatible with {@link orddict} module.

-spec send_one_command(module(), gen_indira_socket:connect_address(),
                       gen_indira_cli:request(), Options :: [Opt]) ->
  {ok, gen_indira_cli:reply()} | {error, Reason}
  when Reason :: bad_request_format | bad_reply_format | term(),
       Opt :: {timeout, timeout()} | retry.

send_one_command(Module, Address, Command, Options) when is_list(Options) ->
  Timeout = proplists:get_value(timeout, Options, infinity),
  Function = case proplists:get_bool(retry, Options) of
    true  -> retry_send_one_line;
    false -> send_one_line
  end,
  case indira_json:encode(Command) of
    {ok, Line} ->
      case Module:Function(Address, Line, Timeout) of
        {ok, ReplyLine} ->
          case indira_json:decode(ReplyLine) of
            {ok, Reply} ->
              {ok, Reply};
            {error, badarg} ->
              {error, bad_reply_format}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, badarg} ->
      {error, bad_request_format}
  end.

%% }}}
%%----------------------------------------------------------

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
%%   `{error, {Reason, Arg}}' is returned to the caller, with `Arg' being the
%%   argument at which the error occurred.
%%
%%   Anything else than `{take, N, _}' or `{error, _}' is considered to be
%%   a new accumulator. The same can be achieved by returning `{take, 1, _}'.
%%
%%   If `Fun' is two-argument function, iteration over arguments list has no
%%   modifications to it.
%%
%%   If `Fun' is three-argument, each `"--foo=XXX"' argument (leading two
%%   dashes) is split on the first `"="' character and the right side of the
%%   split (`"XXX"') is inserted into arguments list just after the raw option
%%   `"--foo"', so it becames `["--foo", "XXX", RestArgs ...]'. To signal that
%%   this operation was done, `Fun(split, ArgList, Acc)' is called. Otherwise,
%%   `Fun(simple, ArgList, Acc)' is called.
%%
%%   If `Fun(split, _, Acc)' doesn't return `{take, N, NewAcc}', it results in
%%   an error (`{error, {excessive_value, Arg}}'). However, `Fun' can decide
%%   it is OK to only consume option and leave its argument in the argument
%%   list by returning `{take, 1, _}'.

-spec foldg(FoldFunction, term(), [string()]) ->
  {ok, NewAcc :: term()} | {error, {FoldError | term(), Arg :: string()}}
  when FoldError :: excessive_value,
       FoldFunction :: SimpleFoldFunction | SplitFoldFunction,
       SimpleFoldFunction :: fun(
         (ArgList :: [string(), ...], Acc :: term()) ->
             NewAcc :: term()
           | {take, pos_integer(), NewAcc :: term()}
           | {error, term()}
         ),
       SplitFoldFunction :: fun(
         (single | split, ArgList :: [string(), ...], Acc :: term()) ->
             NewAcc :: term()
           | {take, pos_integer(), NewAcc :: term()}
           | {error, term()}
         ).

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
      {error, {Reason, Arg}};
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
      {error, {Reason, Arg}};
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
      {error, {Reason, Arg}};
    % it is an error to only consume one argument, when in fact it was just
    % a half of the original argument
    _NewAcc ->
      {error, {excessive_value, Arg}}
  end.

%% }}}
%%----------------------------------------------------------

%% @doc Simple fold over command line arguments.
%%
%%   Function `Fun' is called with only a single command line argument at
%%   a time. If the function returns `{need, N}', it will be immediately
%%   called with a list of `N+1' arguments (current and the next `N'), (unless
%%   the list of remaining arguments is shorter than `N', in which case the
%%   whole iteration is terminated with `{error, {not_enough_args, Arg}}'
%%   result).
%%
%%   As with {@link foldg/3}, `Fun' returning `{error, Reason}' terminates the
%%   iteration with result of `{error, {Reason, Arg}}' (on the call after
%%   `{need, N}', only the first element of the list is used).

-spec folds(FoldFunction, term(), [string()]) ->
  {ok, NewAcc :: term()} | {error, {FoldError | term(), Arg :: string()}}
  when FoldError :: not_enough_args,
       FoldFunction :: fun(
         (Arg :: string() | [string(), ...], Acc :: term()) ->
           NewAcc :: term() | {need, pos_integer()} | {error, term()}
       ).

folds(_Fun, Acc, [] = _ArgList) ->
  {ok, Acc};
folds(Fun, Acc, [Arg | RestArgs] = _ArgList) ->
  case Fun(Arg, Acc) of
    {need, N} when is_integer(N), N > 0 ->
      folds_second_call(Fun, Acc, N, Arg, RestArgs);
    {error, Reason} ->
      {error, {Reason, Arg}};
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
        {error, Reason} -> {error, {Reason, Opt}};
        NewAcc -> folds(Fun, NewAcc, RestArgs)
      end;
    error ->
      {error, {not_enough_args, Opt}}
  end.

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
