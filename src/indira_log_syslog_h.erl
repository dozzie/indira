%%%---------------------------------------------------------------------------
%%% @doc
%%%   Syslog log handler.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_log_syslog_h).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).

-record(state, {facility, ident, syslog}).

%%%---------------------------------------------------------------------------
%%% gen_event callbacks
%%%---------------------------------------------------------------------------

%% @private
%% @doc Initialize event handler.
%%   `Args' is a proplist that should contain `facility' and `ident' keys and
%%   may contain `remote' key. `remote' specifies UDP syslog to send logs to.
%%
%% @spec init([{Name :: atom(), Value :: term()} | atom()]) ->
%%   {ok, term()}

init(Args) ->
  Facility = proplists:get_value(facility, Args, daemon),
  Ident = proplists:get_value(ident, Args, erlang),
  Syslog = case proplists:lookup(remote, Args) of
    none ->
      indira_syslog:open_local("/dev/log");
    {remote, {Host, Port}} ->
      indira_syslog:open_remote(Host, Port);
    {remote, Host} -> % can be tuple, too (IP address)
      indira_syslog:open_remote(Host, default)
  end,
  {ok, #state{facility = Facility, ident = Ident, syslog = Syslog}}.

%% @private
%% @doc Clean up after event handler.
terminate(_Arg, _State = #state{syslog = Syslog}) ->
  indira_syslog:close(Syslog),
  ok.

%% @private
%% @doc Handle incoming events.
%% @end
%%
%% message:
%%   Pid  :: pid() -- process sending an event
%%   GL   :: pid() -- group leader
%%   Type :: std_error | std_warning | std_info | term() -- type of `*_report'

handle_event({error, _GL, {Pid, Format, Data}} = _Event, State) ->
  error(Pid, message, io_lib:format(Format, Data), State),
  {ok, State};
handle_event({error_report, _GL, {Pid, Type, Data}} = _Event, State) ->
  error(Pid, Type, format_report(Data), State),
  {ok, State};

handle_event({warning_msg, _GL, {Pid, Format, Data}} = _Event, State) ->
  warning(Pid, message, io_lib:format(Format, Data), State),
  {ok, State};
handle_event({warning_report, _GL, {Pid, Type, Data}} = _Event, State) ->
  warning(Pid, Type, format_report(Data), State),
  {ok, State};

handle_event({info_msg, _GL, {Pid, Format, Data}} = _Event, State) ->
  info(Pid, message, io_lib:format(Format, Data), State),
  {ok, State};
handle_event({info_report, _GL, {Pid, Type, Data}} = _Event, State) ->
  info(Pid, Type, format_report(Data), State),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

%% @private
%% @doc Handle {@link gen_event:call/3}.
handle_call(_Request, State) ->
  {ok, ok, State}.

%% @private
%% @doc Handle incoming messages.
handle_info(_Message, State) ->
  {ok, State}.

%% @private
%% @doc Handle code change.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%---------------------------------------------------------------------------

%% @doc Print error message.
%%   Function uses passed group leader to send characters to.
error(_Pid, _LogType, Line,
      _State = #state{facility = Facility, ident = Ident, syslog = Syslog}) ->
  % TODO: add Pid and LogType to event context
  Message = indira_syslog:format(Facility, err, Ident, Line),
  indira_syslog:send(Syslog, Message),
  ok.

%% @doc Print warning message.
%%   Function uses passed group leader to send characters to.
warning(_Pid, _LogType, Line,
        _State = #state{facility = Facility, ident = Ident, syslog = Syslog}) ->
  % TODO: add Pid and LogType to event context
  Message = indira_syslog:format(Facility, warning, Ident, Line),
  indira_syslog:send(Syslog, Message),
  ok.

%% @doc Print info message.
%%   Function uses passed group leader to send characters to.
info(_Pid, _LogType, Line,
     _State = #state{facility = Facility, ident = Ident, syslog = Syslog}) ->
  % TODO: add Pid and LogType to event context
  Message = indira_syslog:format(Facility, info, Ident, Line),
  indira_syslog:send(Syslog, Message),
  ok.

%%%---------------------------------------------------------------------------

%% @doc Format report as a single line.
%%   If it's a char list, it's left untouched. Otherwise, it's formatted as
%%   single-line Erlang term.
format_report(Report) ->
  case io_lib:char_list(Report) of
    true -> Report;
    % 16#ffffffff (4G) should be large enough for result not to wrap
    false -> io_lib:print(Report, 1, 16#ffffffff, -1)
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
