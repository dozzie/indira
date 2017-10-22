%%%---------------------------------------------------------------------------
%%% @doc
%%%   Handler for {@link error_logger} to write events to a text file.
%%%
%%%   When added with {@link gen_event:add_handler/3} function, this module
%%%   expects a one-element list containing output file name:
%```
%gen_event:add_handler(error_logger, indira_disk_h, [Filename]).
%'''
%%%
%%%   This module can be used before Indira application starts.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_disk_h).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).

-export([install/2, reopen/2, remove/1]).
-export([install/3, reopen/3, remove/2]).
-export([format_error/1]).

-export_type([event/0]).
-export_type([type_message/0, event_message/0]).
-export_type([type_report/0, event_report/0, report_type/0, report/0]).

%%%---------------------------------------------------------------------------

-define(MAX_LINE_LENGTH, 16#ffffffff). % 4GB should be enough for a log line

-record(state, {
  filename :: file:filename(),
  handle :: file:io_device()
}).

-type type_message() :: error | warning_msg | info_msg.
%% Marker of a formatted log message ({@type event_message()}).

-type type_report() :: error_report | warning_report | info_report.
%% Marker of a structured log message ({@type event_report()}).

-type report_type() :: std_error | std_warning | std_info | term().
%% Log level or custom category of a {@type report()}.

-type report() :: [{Tag :: term(), Data :: term()} | term()]
                  | string() | term().
%% Structured log payload.

-type event_message() :: {pid() | atom(), Format :: string(), Args :: [term()]}.
%% Formatted log event generated using {@link error_logger:error_msg/2},
%% {@link error_logger:warning_msg/2}, or {@link error_logger:info_msg/2}.
%%
%% `Format' and `Args' are arguments suitable for {@link io:format/2}.

-type event_report()  :: {pid() | atom(), report_type(), report()}.
%% Structured log event generated using {@link error_logger:error_report/1},
%% {@link error_logger:warning_report/1}, {@link error_logger:info_report/1},
%% or their two-argument counterparts.

-type event() ::
    {type_message(), GroupLeader :: pid(), event_message()}
  | {type_report(), GroupLeader :: pid(), event_report()}.
%% {@link error_logger} event, either formatted or structured.

%%%---------------------------------------------------------------------------

%% @doc Install log handler to an event manager.
%%
%%   The handler will be added with `indira_disk_h' name.
%%
%%   `EventManager' will usually be `error_logger'.

-spec install(pid() | atom(), file:filename()) ->
  ok | {error, file:posix() | badarg | system_limit}.

install(EventManager, File) ->
  gen_event:add_handler(EventManager, ?MODULE, [File]).

%% @doc Install log handler to an event manager.
%%
%%   The handler will be added with `{indira_disk_h, Id}' name. This is for
%%   adding more than one log handler to a single `EventManager'.
%%
%%   `EventManager' will usually be `error_logger'.

-spec install(pid() | atom(), term(), file:filename()) ->
  ok | {error, file:posix() | badarg | system_limit}.

install(EventManager, Id, File) ->
  gen_event:add_handler(EventManager, {?MODULE, Id}, [File]).

%% @doc Remove from an event manager log handler installed with
%%   {@link install/2}.
%%
%%   `EventManager' will usually be `error_logger'.

-spec remove(pid() | atom()) ->
  ok | {error, module_not_found}.

remove(EventManager) ->
  gen_event:delete_handler(EventManager, ?MODULE, []).

%% @doc Remove from an event manager log handler installed with
%%   {@link install/3}.
%%
%%   `EventManager' will usually be `error_logger'.

-spec remove(pid() | atom(), term()) ->
  ok | {error, module_not_found}.

remove(EventManager, Id) ->
  gen_event:delete_handler(EventManager, {?MODULE, Id}, []).

%% @doc Close old file and open new one in the log handler.
%%   If the handler was not present, it will be added.
%%
%%   `EventManager' will usually be `error_logger'.
%%
%%   This function is intended for handlers added by {@link install/2} (or
%%   with `indira_disk_h' as a handler name).

-spec reopen(pid() | atom(), file:filename()) ->
  ok | {error, file:posix() | badarg | system_limit}.

reopen(EventManager, File) ->
  case gen_event:call(EventManager, ?MODULE, {reopen, File}) of
    ok ->
      ok;
    {error, bad_module} ->
      gen_event:add_handler(EventManager, ?MODULE, [File]);
    {error, Reason} ->
      {error, Reason};
    {'EXIT', Reason} ->
      {error, {'EXIT', Reason}}
  end.

%% @doc Close old file and open new one in the log handler.
%%   If the handler was not present, it will be added.
%%
%%   `EventManager' will usually be `error_logger'.
%%
%%   This function is intended for handlers added by {@link install/3} (or
%%   with `{indira_disk_h, Id}' as a handler name).

-spec reopen(pid() | atom(), term(), file:filename()) ->
  ok | {error, file:posix() | badarg | system_limit}.

reopen(EventManager, Id, File) ->
  case gen_event:call(EventManager, {?MODULE, Id}, {reopen, File}) of
    ok ->
      ok;
    {error, bad_module} ->
      gen_event:add_handler(EventManager, {?MODULE, Id}, [File]);
    {error, Reason} ->
      {error, Reason};
    {'EXIT', Reason} ->
      {error, {'EXIT', Reason}}
  end.

%%%---------------------------------------------------------------------------
%%% gen_event callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init([File] = _Args) ->
  case file:open(File, [append, raw, delayed_write]) of
    {ok, Handle} ->
      State = #state{
        filename = File,
        handle = Handle
      },
      {ok, State};
    {error, Reason} ->
      {error, Reason}
  end.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Arg, _State = #state{handle = Handle}) ->
  file:close(Handle),
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_event:notify/2}.

handle_event({LogType, _GroupLeader, LogData} = _Event,
             State = #state{handle = Handle}) ->
  case format_event(LogType, LogData) of
    {ok, Line} ->
      case file:write(Handle, [Line, $\n]) of
        ok -> ok;
        {error, edquot} -> ok;
        {error, enospc} -> ok;
        {error, enomem} -> ok;
        {error, _Reason} -> remove_handler % TODO: log this?
      end;
    skip ->
      skip
  end,
  {ok, State};

%% unknown events
handle_event(_Event, State) ->
  {ok, State}.

%% @private
%% @doc Handle {@link gen_event:call/2}.

handle_call({reopen, NewFile} = _Request, State = #state{handle = Handle}) ->
  file:close(Handle),
  case file:open(NewFile, [append, raw, delayed_write]) of
    {ok, NewHandle} ->
      NewState = State#state{
        filename = NewFile,
        handle = NewHandle
      },
      {ok, ok, NewState};
    {error, Reason} ->
      % TODO: log this?
      {remove_handler, {error, Reason}}
  end;

%% unknown calls
handle_call(_Request, State) ->
  {ok, {error, unknown_call}, State}.

%% @private
%% @doc Handle incoming messages.

%% unknown messages
handle_info(_Message, State) ->
  {ok, State}.

%% }}}
%%----------------------------------------------------------
%% code change {{{

%% @private
%% @doc Handle code change.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% helpers
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% event formatting {{{

%% @doc Format {@link error_logger} event for writing in a log file.

-spec format_event(type_message() | type_report(),
                   event_message() | event_report()) ->
  {ok, iolist()} | skip.

format_event(LogType, LogData) ->
  Timestamp = timestamp(),
  case level_type(LogType) of
    {Level, format = _Type} ->
      case format(LogData) of
        {ok, Process, Line} ->
          LogPrefix = log_prefix(Timestamp, Level, Process),
          {ok, [LogPrefix, " ", Line]};
        {error, _Reason} ->
          skip
      end;
    {Level, report = _Type} ->
      case report(LogData) of
        {ok, Process, Line} ->
          LogPrefix = log_prefix(Timestamp, Level, Process),
          {ok, [LogPrefix, " ", Line]};
        {error, _Reason} ->
          skip
      end;
    {error, badarg} ->
      skip
  end.

%% @doc Build a prefix for a log line.

-spec log_prefix(integer(), error | warning | info, pid() | atom()) ->
  iolist().

log_prefix(Time, Level, Process) when is_atom(Process) ->
  [integer_to_list(Time), " ", atom_to_list(Level), " ",
    "[", os:getpid(), "] ", atom_to_list(Process)];
log_prefix(Time, Level, Process) when is_pid(Process) ->
  [integer_to_list(Time), " ", atom_to_list(Level), " ",
    "[", os:getpid(), "] ", pid_to_list(Process)].

%% @doc Convert a tag to a log level and its type.

-spec level_type(type_message() | type_report()) ->
  {Level, Type} | {error, badarg}
  when Level :: error | warning | info,
       Type :: format | report.

level_type(error)          -> {error, format};
level_type(error_report)   -> {error, report};
level_type(warning_msg)    -> {warning, format};
level_type(warning_report) -> {warning, report};
level_type(info_msg)       -> {info, format};
level_type(info_report)    -> {info, report};
level_type(_) -> {error, badarg}.

%% @doc Fill a format string with data, making it a log line.

-spec format(event_message()) ->
  {ok, pid() | atom(), iolist()} | {error, badarg | term()}.

format({Process, Format, Args} = _LogData) ->
  try
    Line = io_lib:format(Format, Args),
    {ok, Process, Line}
  catch
    error:Reason ->
      {error, Reason}
  end;
format(_LogData) ->
  {error, badarg}.

%% @doc Format a report, making it a log line.

-spec report(event_report()) ->
  {ok, pid() | atom(), iolist()} | {error, badarg}.

report({Process, Type, Report} = _LogData) ->
  Line = [
    io_lib:print(Type, 1, ?MAX_LINE_LENGTH, -1),
    " ",
    io_lib:print(Report, 1, ?MAX_LINE_LENGTH, -1)
  ],
  {ok, Process, Line};
report(_LogData) ->
  {error, badarg}.

%% @doc Get a log timestamp.

-spec timestamp() ->
  integer().

timestamp() ->
  {MS, S, _US} = os:timestamp(), % good enough for logging
  MS * 1000 * 1000 + S.

%% }}}
%%----------------------------------------------------------
%% format errors {{{

%% @doc Format an error reported by this module.

-spec format_error(term()) ->
  string().

format_error(Reason) ->
  file:format_error(Reason).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
