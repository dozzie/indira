%%%---------------------------------------------------------------------------
%%% @doc
%%%   Logging for Indira processes with unified messages.
%%%
%%%   Events logged with this module are all sent to {@link error_logger}.
%%%
%%%   This module is intended to be used by programmers writing custom
%%%   {@link gen_indira_listener} modules.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_log).

-export([info/2, error/2, error/3, critical/2, critical/3]).

-export_type([event/0, context/0, error_reason/0]).
-export_type([error_logger_event/0]).

%%%---------------------------------------------------------------------------

-type event() :: atom().
%% Short description of what event is about.

-type error_reason() :: term().
%% `Reason' part of a `{error, Reason}' tuple usually returned in case of
%% errors.

-type context() :: [{Key :: atom(), Value :: term()}].
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
%%% API
%%%---------------------------------------------------------------------------

%% @doc Send info report about an event to {@link error_logger}.
%%   The report is formatted uniformly for Indira.

-spec info(event(), context()) ->
  ok.

info(InfoType, Context) ->
  error_logger:info_report([{indira_info, InfoType} | Context]).

%% @doc Send error report about an error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use with single `{error,Reason}' tuple.
%%
%%   Such error is something that generally shouldn't happen, but if it does,
%%   it has limited scope (e.g. command line parse error for a single client).

-spec error(event(), error_reason(), context()) ->
  ok.

error(ErrorType, ErrorReason, Context) ->
  error_logger:warning_report(
    [{indira_error, ErrorType}, {error, ErrorReason} | Context]
  ).

%% @doc Send error report about an error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use when no single `{error,Reason}' tuple
%%   is present (e.g. when multiple errors occurred).
%%
%%   Such error is something that generally shouldn't happen, but if it does,
%%   it has limited scope (e.g. command line parse error for a single client).

-spec error(event(), context()) ->
  ok.

error(ErrorType, Context) ->
  error_logger:warning_report([{indira_error, ErrorType} | Context]).

%% @doc Send log report about severe error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use with single `{error,Reason}' tuple.
%%
%%   For severe errors, like inability to listen on a specified port.

-spec critical(event(), error_reason(), context()) ->
  ok.

critical(ErrorType, ErrorReason, Context) ->
  error_logger:error_report(
    [{indira_error, ErrorType}, {error, ErrorReason} | Context]
  ).

%% @doc Send log report about severe error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use when no single `{error,Reason}' tuple
%%   is present (e.g. when multiple errors occurred).
%%
%%   For severe errors, like inability to listen on a specified port.

-spec critical(event(), context()) ->
  ok.

critical(ErrorType, Context) ->
  error_logger:error_report([{indira_error, ErrorType} | Context]).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
