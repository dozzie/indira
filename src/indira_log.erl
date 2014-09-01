%%%---------------------------------------------------------------------------
%%% @doc
%%%   Logging for Indira processes with unified messages.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_log).

-export([info/2, error/2, error/3, critical/2, critical/3]).

%%%---------------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------------

%% @doc Send info report about an event to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
info(InfoType, Context) ->
  error_logger:info_report([{indira_info, InfoType} | Context]).

%% @doc Send error report about an error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use with single `{error,Reason}' tuple.
%%
%%   Such error is something that generally shouldn't happen, but if it does,
%%   it has limited scope (e.g. command line parse error for a single client).
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
error(ErrorType, Context) ->
  error_logger:warning_report([{indira_error, ErrorType} | Context]).

%% @doc Send log report about severe error to {@link error_logger}.
%%   The report is formatted uniformly for Indira.
%%
%%   This function is intended for use with single `{error,Reason}' tuple.
%%
%%   For severe errors, like inability to listen on a specified port.
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
critical(ErrorType, Context) ->
  error_logger:error_report([{indira_error, ErrorType} | Context]).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
