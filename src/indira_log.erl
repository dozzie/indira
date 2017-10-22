%%%---------------------------------------------------------------------------
%%% @doc
%%%   Logging for Indira processes with unified messages.
%%%
%%%   This module is intended to be used by programmers writing custom
%%%   {@link gen_indira_socket} modules.
%%%
%%%   Events logged with this module are all sent to {@link error_logger}.
%%%   They will be logged as info/warning/error reports with type
%%%   {@type @{indira, Type@}} and report being a {@type log_context()} with
%%%   at least {@type @{message, string()@}} entry.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_log).

%% pre-set context
-export([set_context/2, get_context/0]).
-export([info/2, warn/2, crit/2]).
%% general interface
-export([info/3, warn/3, crit/3]).

-export_type([event_type/0, log_context/0]).
-export_type([error_logger_event/0]).

%%%---------------------------------------------------------------------------

-type event_type() :: atom().
%% Short description of what the event is about. A subsystem or module name
%% makes a good event type.

-type log_context() :: [{Key :: atom(), Value :: term()}].
%% More informations about the event, including a human-readable message.

-type error_logger_event() ::
    {info_msg | warning_msg | error,
      GroupLeader :: pid(),
      {EventOrigin :: pid(), Format :: string() | atom(), Data :: [term()]}}
  | {info_report | warning_report | error_report,
      {EventOrigin :: pid(), Type :: atom(), Report :: term()}}.
%% Event structure extracted from {@link error_logger} documentation.
%%
%% Note that while {@link error_logger:info_msg/1} and
%% {@link error_logger:warning_msg/1} produce `*_msg' events,
%% {@link error_logger:error_msg/1} produces just `error'.
%% `error_report' keeps the convention, however.
%%
%% `EventOrigin' is the process that generated event (that is, called
%% {@link error_logger:info_msg/1} or its companion).
%%
%% `Type' is a type of report. Some typical values include:
%% <ul>
%%   <li>`std_info', `std_warning', `std_error' for
%%       {@link error_logger:info_report/1},
%%       {@link error_logger:warning_report/1},
%%       {@link error_logger:error_report/1}</li>
%%   <li>`progress' for application start report</li>
%%   <li>`supervisor_report' for errors from supervisor</li>
%% </ul>
%% `Type' in error report can be specified by calling
%% {@link error_logger:error_report/2} (and similar thing for info and
%% warning).

%%%---------------------------------------------------------------------------
%%% pre-set context
%%%---------------------------------------------------------------------------

%% @doc Set log context for {@link info/2}, {@link warn/2}, and
%%   {@link crit/2}.
%%
%%   This allows to pre-format and store some information about the process,
%%   especially for cases when this information wouldn't be available down in
%%   the call stack if it wasn't for logging.
%%
%%   Context is remembered in process dictionary.
%%
%% @see get_context/0

-spec set_context(event_type(), log_context()) ->
  ok.

set_context(Type, Context) when is_atom(Type), is_list(Context) ->
  put('$indira_log', {Type, Context}),
  ok.

%% @doc Read log context set with {@link set_context/2}.

-spec get_context() ->
  {event_type(), log_context()} | undefined.

get_context() ->
  get('$indira_log').

%% @doc Event of informative significance.
%%   New client connected, cancelled an order, submitted new job, etc.
%%
%%   Uses context and event type set with {@link set_context/2}.

-spec info(string(), log_context()) ->
  ok.

info(Message, Info) when is_list(Info) ->
  {Type, Context} = get_context(),
  info(Type, Message, Info ++ Context).

%% @doc Minor error event.
%%   Typically the error has its cause in some remote entity (e.g. protocol
%%   error), but it's harmless for the operation of Indira or daemon itself.
%%
%%   Uses context and event type set with {@link set_context/2}.

-spec warn(string(), log_context()) ->
  ok.

warn(Message, Info) when is_list(Info) ->
  {Type, Context} = get_context(),
  warn(Type, Message, Info ++ Context).

%% @doc Major error event.
%%   An unexpected event that should never occur, possibly threatening service
%%   operation (or part of it).
%%
%%   Uses context and event type set with {@link set_context/2}.

-spec crit(string(), log_context()) ->
  ok.

crit(Message, Info) when is_list(Info) ->
  {Type, Context} = get_context(),
  crit(Type, Message, Info ++ Context).

%%%---------------------------------------------------------------------------
%%% general interface
%%%---------------------------------------------------------------------------

%% @doc Event of informative significance.
%%   New client connected, cancelled an order, submitted new job, etc.

-spec info(event_type(), string(), log_context()) ->
  ok.

info(Type, Message, Context) when is_atom(Type), is_list(Context) ->
  error_logger:info_report({indira, Type}, [{message, Message} | Context]).

%% @doc Minor error event.
%%   Typically the error has its cause in some remote entity (e.g. protocol
%%   error), but it's harmless for the operation of Indira or daemon itself.

-spec warn(event_type(), string(), log_context()) ->
  ok.

warn(Type, Message, Context) when is_atom(Type), is_list(Context) ->
  error_logger:warning_report({indira, Type}, [{message, Message} | Context]).

%% @doc Major error event.
%%   An unexpected event that should never occur, possibly threatening service
%%   operation (or part of it).

-spec crit(event_type(), string(), log_context()) ->
  ok.

crit(Type, Message, Context) when is_atom(Type), is_list(Context) ->
  error_logger:error_report({indira, Type}, [{message, Message} | Context]).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
