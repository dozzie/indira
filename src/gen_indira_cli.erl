%%%---------------------------------------------------------------------------
%%% @doc
%%%   Command line arguments handler.
%%%
%%%   Module implementing this behaviour will be used for parsing arguments
%%%   passed in command line, decoding from them an operation to be performed
%%%   (usually starting daemon and sending it one of supported administrative
%%%   commands), and executing it.
%%%
%%%   == Expected typical usage ==
%%%
%%%   === command line script ===
%%%
%%%   `gen_indira_cli' behaviour is intended to structure processing command
%%%   line arguments. {@link gen_indira_cli:execute/3} facilitates this in
%%%   `escript' scripts, (`escript' is a part of runtime that is well-suited
%%%   for this task, but not the only one). Such a script could look like
%%%   this:
%%%
%```
%#!/usr/bin/escript
%
%main(Args) ->
%  AdminSocket = "/var/run/example_app/control",
%  PidFile     = "/var/run/example_app/pid",
%  ConfigFile  = "/etc/example_app.conf",
%  Defaults = [AdminSocket, PidFile, ConfigFile],
%  case gen_indira_cli:execute(Args, example_cli, Defaults) of
%    ok ->
%      ok;
%    help ->
%      io:fwrite("~s~n", [example_cli:usage()]);
%    {error, {arguments, Reason}} ->
%      io:fwrite(standard_error, "~p~n", [Reason]), % not a pretty message
%      io:fwrite(standard_error, "~s~n", [example_cli:usage()]);
%      halt(1);
%    {error, Reason} ->
%      io:fwrite(standard_error, "~p~n", [Reason]), % not a pretty message
%      halt(1)
%  end.
%'''
%%%
%%%   === command line handler module ===
%%%
%%%   The other side is a module that contains operations' code, i.e. the one
%%%   implementing `gen_indira_cli' behaviour.
%%%
%%%   Most of the commands are expected to be sent to daemon instance through
%%%   administrative connection ({@link gen_indira_socket}), though the
%%%   details may vary (e.g. reaction to refused connections).
%%%
%%%   The single distinguished operation is to start the application that is
%%%   the core of a daemon. This may be done using {@link application:start/2}
%%%   or {@link indira:start_rec/2} (though one needs to remember to configure
%%%   and start Indira). There is also {@link indira:daemonize/2}, which
%%%   simplifies starting necessary applications a little.
%%%
%%%   Starting a daemon could look like this:
%%%
%```
%-module(example_cli).
%-behaviour(gen_indira_cli).
%...
%
%handle_command(start = _Command, Options) ->
%  AdminSocket = get_admin_socket(Options),
%  case configure_application(Options) of
%    ok ->
%      indira:daemonize(example_app, [
%        {listen, [{indira_unix, AdminSocket}]},
%        {command, {example_command_handler, []}}
%      ]);
%    {error, Reason} ->
%      {error, Reason}
%  end.
%
%parse_arguments(Args, [AdminSocket, PidFile, ConfigFile] = _Defaults) ->
%  case gen_indira_cli:folds(...) of
%    {ok, {start, Options}} -> {ok, start, Options};
%    ...
%  end.
%'''
%%%
%%%   == Expected callbacks ==
%%%
%%%   <ul>
%%%     <li>`parse_arguments(Args, DefaultValues)' -- determine what operation
%%%         to execute from arguments passed in command line
%%%
%%%       Returned value:
%%%       <ul>
%%%         <li>{@type @{ok, command(), options()@}} -- execute a more complex
%%%             command by calling `handle_command()'</li>
%%%         <li>{@type @{send, socket_address(), command(), options()@}} --
%%%             execute a simple command by calling `format_request()',
%%%             sending returned request through administrative socket, and
%%%             calling `handle_reply()' on the reply</li>
%%%         <li>{@type help} -- print a help message to screen (e.g.
%%%             <i>--help</i> option was provided)</li>
%%%         <li>{@type @{error, Reason :: term()@}} -- signal an erroneous
%%%             command line; returned as `{error, {arguments, Reason}}' from
%%%             {@link execute/3}</li>
%%%       </ul>
%%%
%%%       Arguments:
%%%       <ul>
%%%         <li>`Args' ({@type [string()]}) -- arguments passed in command
%%%             line</li>
%%%         <li>`DefaultValues' ({@type term()}) -- arbitrary term passed to
%%%             {@link execute/3} that mainly allows move hardcoded paths from
%%%             module to `escript' script</li>
%%%       </ul>
%%%     </li>
%%%     <li>`handle_command(Command, Options)' -- execute a command, for which
%%%         simple track of sending a request with {@link send_one_command/4}
%%%         and processing reply is not enough (e.g. starting the daemon
%%%         itself); function returns {@type ok} or {@type @{error, term()@}}
%%%         (returned verbatim to {@link execute/3} caller)
%%%
%%%       Arguments:
%%%       <ul>
%%%         <li>`Command' ({@type command()}) -- command to be executed</li>
%%%         <li>`Options' ({@type options()}) -- options set in command
%%%             line</li>
%%%       </ul>
%%%     </li>
%%%     <li>`format_request(Command, Options)' -- encode a command as
%%%         a JSON-serializable structure, so it can be sent through
%%%         administrative socket; function returns {@type @{ok, request()@}}
%%%         or {@type @{error, Reason :: term()@}} (returned as `{error,
%%%         {format, Reason}}' to {@link execute/3} caller)
%%%
%%%       Arguments:
%%%       <ul>
%%%         <li>`Command' ({@type command()}) -- command to be sent to
%%%             daemon</li>
%%%         <li>`Options' ({@type options()}) -- options set in command
%%%             line</li>
%%%       </ul>
%%%     </li>
%%%     <li>`handle_reply(Reply, Command, Options)' -- process a reply to
%%%         a command sent to daemon; function returns {@type ok} or {@type
%%%         @{error, term()@}} (returned verbatim to {@link execute/3} caller)
%%%
%%%       Arguments:
%%%       <ul>
%%%         <li>`Reply' ({@type reply()}) -- reply to `Command' received from
%%%             daemon; all hashes in `Reply' are compatible with {@link
%%%             orddict} module</li>
%%%         <li>`Command' ({@type command()}) -- command (the original one,
%%%             returned from `parse_arguments()') that was sent to
%%%             daemon</li>
%%%         <li>`Options' ({@type options()}) -- options set in command
%%%             line</li>
%%%       </ul>
%%%     </li>
%%%   </ul>
%%%
%%% @see gen_indira_command
%%% @see execute/3
%%% @see indira:daemonize/2
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_indira_cli).

-export([execute/3]).
-export([folds/3, foldg/3]).
-export([send_one_command/4]).

-export_type([command/0, options/0, request/0, reply/0, socket_address/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

-type command() :: term().
%% Command to execute. Usually an atom is enough to describe what to do.

-type options() :: term().
%% Options that change details of the {@type command()}, like administrative
%% socket location, config file, etc.

-type request() :: indira_json:struct().
%% {@type command()} encoded as a JSON-serializable structure, ready to be
%% sent to daemon instance. See also {@link indira_json}.

-type reply() :: indira_json:struct().
%% Reply received from daemon to an administrative command. See also {@link
%% indira_json}.

-type socket_address() :: {module(), gen_indira_socket:connect_address()}.
%% A {@link gen_indira_socket} module and an address for it to use with
%% `send_one_line()' and `retry_send_one_line()'.

%%% }}}
%%%---------------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% behaviour callbacks
%%%---------------------------------------------------------------------------

-callback parse_arguments(Args :: [string()], Defaults :: term()) ->
    {ok, command(), options()}
  | {send, socket_address(), command(), options()}
  | help
  | {error, term()}.

-callback handle_command(Command :: command(), Options :: options()) ->
  ok | {error, term()}.

-callback format_request(Command :: command(), Options :: options()) ->
  {ok, request()} | {error, term()}.

-callback handle_reply(Reply :: reply(), Command :: command(),
                       Options :: options()) ->
  ok | {error, term()}.

%%%---------------------------------------------------------------------------
%%% command line execution
%%%---------------------------------------------------------------------------

%% @doc Execute an operation specified in command line using callback module.
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

-spec execute_command(module(), command(), options()) ->
  ok | {error, term()}.

execute_command(CLIHandler, Command, Options) ->
  case CLIHandler:handle_command(Command, Options) of
    ok -> ok;
    {error, Reason} -> {error, Reason};
    Result -> {error, {bad_return_value, Result}}
  end.

%% @doc Send a simple command to daemon and pass the reply to CLI module.

-spec send_command(module(), command(), options(), socket_address()) ->
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
                       request(), Options :: [Opt]) ->
  {ok, reply()} | {error, Reason}
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
          case indira_json:decode(unicode:characters_to_list(ReplyLine)) of
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
