%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Flat file log handler for {@link error_logger}.
%%%
%%%   == Usage ==
%%%
%```
%error_logger:add_report_handler(indira_log_file_h, [Ident, File]).
%gen_event:add_handler(error_logger, indira_log_file_h, [Ident, File]).
%'''
%%%
%%%   Arguments to the module (as in {@link gen_event:add_handler/3}):
%%%   <ul>
%%%     <li>`[Ident, File]', where `Ident' is atom or string and `File' is
%%%         path (string)</li>
%%%   </ul>
%%%
%%%   <b>NOTE</b>: This module is a clone of {@link indira_log_syslog_h}. Any
%%%   changes to code in <i>communication</i> section should be made first
%%%   there.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_log_file_h).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------
%%% types

-record(state, {ident, file, fh}).

%%%---------------------------------------------------------------------------
%%% gen_event callbacks

%%----------------------------------------------------------
%% initialization and cleanup {{{

%% @private
%% @doc Initialize {@link gen_event} state.

init([Ident, File] = _Args) ->
  ABSFile = filename:absname(File),
  {ok, FH} = file:open(ABSFile, [write]),
  State = #state{ident = Ident, file = ABSFile, fh = FH},
  {ok, State}.

%% @private
%% @doc Clean up {@link gen_event} state.

terminate(_Reason, _State = #state{fh = FH}) ->
  file:close(FH),
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_event:notify/2}.

%% log entry emitted when can't connect to epmd starts with:
%%   Protocol: "inet_tcp": register error: ...
%% also some other errors, but not structured
handle_event({info_msg, _GLead, {_Pid, _Format, _Data}} = _Event, State) ->
  {ok, State}; % ignore

%% unspecified log message (OTP doesn't emit these)
handle_event({warning_msg, _GLead, {_Pid, _Format, _Data}} = _Event, State) ->
  {ok, State}; % ignore

%% log entry emitted when a gen_server crashes
handle_event({error_msg, _GLead, {_Pid, _Format, _Data}} = _Event, State) ->
  {ok, State}; % ignore

%% info-level: application started/stopped, child started, Indira INFO
handle_event({info_report, _GLead, {Pid, Type, Report}} = _Event, State) ->
  case {Type,Report} of
    {progress, [{application, App}, {started_at, _AppNode}]} ->
      % application started
      oplog(State, info, "application started", [{application, App}]);

    {progress, [{supervisor, {_SupPid, _SupName}}, {started, _Child}]} ->
      % child started
      % TODO: what is `SupName' when supervisor is not a registered process?
      % TODO: log some operational details, like child's name, PID and MFA
      {ok, State}; % ignore

    {std_info, [{application, App}, {exited, stopped}, {type, _StartType}]} ->
      % application stopped
      oplog(State, info, "application stopped", [{application, App}]);

    {std_info, [{application, App}, {exited, Reason}, {type, _StartType}]} ->
      % application stopped unexpectedly
      oplog(State, err, "application crashed",
            [{application, App}, {reason, normalize_reason(Reason)}]);

    {std_info, [{indira_info, MsgType} | Context]} ->
      % Indira INFO messages
      oplog(State, info, MsgType, [{context, Context}]);

    {_,_} ->
      Message = [{pid, Pid}, {level, info}, {type, Type}, {report, Report}],
      oplog(State, debug, Message)
  end;

%% error-level: crash reports, child start problems, Indira CRITICAL
handle_event({error_report, _GLead, {Pid, Type, Report}} = _Event, State) ->
  case {Type,Report} of
    {crash_report, [_CrashProps, _EmptyList]} ->
      % gen_server (or supervisor) failed to start, gen_server crashed
      % _EmptyList: at least it is expected it's `[]'
      % NOTE: this does not include processes that got `exit(P,Reason)'
      {ok, State}; % ignore

    {supervisor_report, [{supervisor, {_SupPid, _SupName} = SupId},
                          {errorContext, start_error},
                          {reason, Reason}, {offender, ChildProps}]} ->
      oplog(State, err, "process start error",
            [{reason, normalize_reason(Reason)},
              {supervisor, supervisor_info(Pid, SupId)},
              {child, child_info(ChildProps)}]);

    {supervisor_report, [{supervisor, {_SupPid, _SupName} = SupId},
                          {errorContext, child_terminated},
                          {reason, Reason}, {offender, ChildProps}]} ->
      % similar to crash report above, but cleaner MFA specification and is
      % generated even for processes that got exit signal
      TrueReason = normalize_reason(Reason),
      {Level, Message} = case TrueReason of
        normal   -> {info, "process stopped"};
        shutdown -> {info, "process shut down"};
        _        -> {err,  "process crashed"}
      end,
      oplog(State, Level, Message,
            [{reason, TrueReason},
              {supervisor, supervisor_info(Pid, SupId)},
              {child, child_info(ChildProps)}]);

    {supervisor_report, [{supervisor, {_SupPid, _SupName} = SupId},
                          {errorContext, shutdown},
                          {reason, Reason}, {offender, ChildProps}]} ->
      % supervisor is going down because one child (the offender) was
      % restarted more often than supervisor's max frequency (at least this is
      % the most probable reason; see `Reason')
      oplog(State, err, "supervisor shut down",
            [{reason, normalize_reason(Reason)},
              {supervisor, supervisor_info(Pid, SupId)},
              {child, child_info(ChildProps)}]);

    {supervisor_report, [{supervisor, {_SupPid, _SupName} = SupId},
                          {errorContext, shutdown_error},
                          {reason, Reason}, {offender, ChildProps}]} ->
      % supervisor was going down, but there was a problem (e.g. child didn't
      % respond within expected time to shutdown signal)
      oplog(State, err, "supervisor shutdown error",
            [{reason, normalize_reason(Reason)},
              {supervisor, supervisor_info(Pid, SupId)},
              {child, child_info(ChildProps)}]);

    {std_error, [{indira_error, MsgType} | Context]} ->
      oplog(State, crit, MsgType, [{context, Context}]);

    {_,_} ->
      Message = [{pid, Pid}, {level, error}, {type, Type}, {report, Report}],
      oplog(State, err, Message)
  end;

%% warning-level: Indira ERROR
handle_event({warning_report, _GLead, {Pid, Type, Report}} = _Event, State) ->
  case {Type,Report} of
    {std_warning, [{indira_error, MsgType} | Context]} ->
      oplog(State, err, MsgType, [{context, Context}]);

    {_,_} ->
      Message = [{pid, Pid}, {level, warning}, {type, Type}, {report, Report}],
      oplog(State, warning, Message)
  end;

%% any other message: ignore
handle_event(_Event, State) ->
  {ok, State}.

%% @private
%% @doc Handle {@link gen_event:call/3}.

handle_call(state = _Request, State) ->
  {ok, {ok, State}, State};

handle_call(_Request, State) ->
  {ok, {error, unknown}, State}.

%% @private
%% @doc Handle incoming messages.

handle_info(_Msg, State) ->
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

%%%---------------------------------------------------------------------------
%%% helper functions {{{

%%----------------------------------------------------------

%% @doc Write event with context (proplist) to file.
%%
%% @spec oplog(#state{}, atom(), atom() | string() | binary(), list()) ->
%%   {ok, State}

oplog(State, Priority, Event, Context) when is_list(Event) ->
  oplog(State, Priority, [{event, list_to_binary(Event)} | Context]);

oplog(State, Priority, Event, Context) when is_binary(Event); is_atom(Event) ->
  oplog(State, Priority, [{event, Event} | Context]).

%% @doc Write message (any term) to file.
%%
%% @spec oplog(#state{}, atom(), term()) ->
%%   {ok, State}

oplog(State = #state{ident = Ident, fh = FH}, Priority, Msg) ->
  Line = [
    fmtime(), "\t",
    fmtident(Ident), "[", os:getpid(), "]", "\t",
    fmtprio(Priority), "\t",
    fmtmsg(Msg), "\n"
  ],
  ok = file:write(FH, Line),
  {ok, State}.

fmtmsg(Msg) ->
  % 2G columns should be \infty, term should fit in this width
  io_lib:print(Msg, 1, 16#ffffffff, -1).

fmtime() ->
  {{Y,Month,Day},{H,M,S}} = erlang:universaltime(),
  io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B+00:00",
                [Y, Month, Day, H, M, S]).

fmtident(Ident) when is_list(Ident) -> Ident;
fmtident(Ident) when is_atom(Ident) -> atom_to_list(Ident).

fmtprio(err = _Prio)   -> "error";
fmtprio(crit = _Prio)  -> "critical";
%fmtprio(emerg = _Prio) -> "emergency";
fmtprio(Prio)          -> atom_to_list(Prio).

%%----------------------------------------------------------

supervisor_info(Pid, {local, SupName} = _SupId) ->
  Info = [
    {name, SupName},
    {message_origin, pid_to_binary(Pid)}
  ],
  Info;

supervisor_info(Pid, {SupPid, SupName} = _SupId) when is_pid(SupPid) ->
  Info = [
    {pid, pid_to_binary(SupPid)},
    {name, SupName},
    {message_origin, pid_to_binary(Pid)}
  ],
  Info.

%%----------------------------------------------------------

child_info(ChildProps) ->
  collect_child_info(ChildProps).

%%----------------------------------------------------------

collect_child_info([] = _ChildProps) ->
  [];

collect_child_info([{pid, undefined} | RestInfo]) ->
  [{pid, undefined} | collect_child_info(RestInfo)];
collect_child_info([{pid, Pid} | RestInfo]) when is_pid(Pid) ->
  [{pid, pid_to_binary(Pid)} | collect_child_info(RestInfo)];

collect_child_info([{name, undefined} | RestInfo]) ->
  collect_child_info(RestInfo);
collect_child_info([{name, Name} | RestInfo]) ->
  [{name, Name} | collect_child_info(RestInfo)];

collect_child_info([{child_type, Type} | RestInfo]) ->
  [{type, Type} | collect_child_info(RestInfo)];

collect_child_info([_Info | RestInfo]) ->
  collect_child_info(RestInfo).

%%----------------------------------------------------------

pid_to_binary(Pid) when is_pid(Pid) ->
  list_to_binary(pid_to_list(Pid)).

%%----------------------------------------------------------

normalize_reason({{TrueReason, _Value}, Stack} = _Reason)
when is_list(Stack) ->
  % `{badmatch,V}', `{case_clause,V}', `{try_clause,V}', ...
  TrueReason;

normalize_reason({undef, [{MissM,MissF,MissArgs} | _] = _Stack} = _Reason) ->
  % undefined function
  % TODO: FuncName = <<
  %   (atom_to_binary(MissM, utf8))/binary, ":",
  %   (atom_to_binary(MissF, utf8))/binary, "/",
  %   (list_to_binary(integer_to_list(length(MissArgs))))/binary
  % >>
  {undef, {MissM, MissF, length(MissArgs)}};

normalize_reason({TrueReason, Stack} = _Reason) when is_list(Stack) ->
  % process died (with stack trace)
  TrueReason;

normalize_reason({'EXIT', TrueReason} = _Reason) ->
  % `catch(exit(...))'
  TrueReason;

normalize_reason(Reason) ->
  Reason.

%%----------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
