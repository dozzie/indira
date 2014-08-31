%%%---------------------------------------------------------------------------
%%% @doc
%%%   {@link error_logger} log handler (print to <i>STDOUT</i>).
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_log_tty_h).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).

-record(opts, {
  iodev, % 'group_leader' | 'user' | 'standard_error' | pid() to I/O server
  colour = false
}).

%%%---------------------------------------------------------------------------
%%% gen_event callbacks
%%%---------------------------------------------------------------------------

%% @private
%% @doc Initialize event handler.
%%   `Args' is a proplist that can contain `colour' or `color' boolean key. If
%%   it's `true', console output will be coloured (default is `false').
%%
%% @spec init([{Name :: atom(), Value :: term()} | atom()]) ->
%%   {ok, term()}

init(Args) ->
  Colour = proplists:get_bool(colour, Args) orelse
           proplists:get_bool(color, Args),
  IODev = proplists:get_value(iodev, Args, group_leader),
  {ok, #opts{iodev = IODev, colour = Colour}}.

%% @private
%% @doc Clean up after event handler.
terminate(_Arg, _Opts) ->
  ok.

%% @private
%% @doc Handle incoming events.
%% @end
%%
%% message:
%%   Pid  :: pid() -- process sending an event
%%   GL   :: pid() -- group leader
%%   Type :: std_error | std_warning | std_info | term() -- type of `*_report'

handle_event({error, GL, {Pid, Format, Data}} = _Event, Opts) ->
  Target = target(GL, Opts),
  error(Target, Pid, message, io_lib:format(Format, Data), Opts),
  {ok, Opts};
handle_event({error_report, GL, {Pid, Type, Data}} = _Event, Opts) ->
  Target = target(GL, Opts),
  error(Target, Pid, Type, format_report(Data), Opts),
  {ok, Opts};

handle_event({warning_msg, GL, {Pid, Format, Data}} = _Event, Opts) ->
  Target = target(GL, Opts),
  warning(Target, Pid, message, io_lib:format(Format, Data), Opts),
  {ok, Opts};
handle_event({warning_report, GL, {Pid, Type, Data}} = _Event, Opts) ->
  Target = target(GL, Opts),
  warning(Target, Pid, Type, format_report(Data), Opts),
  {ok, Opts};

handle_event({info_msg, GL, {Pid, Format, Data}} = _Event, Opts) ->
  Target = target(GL, Opts),
  info(Target, Pid, message, io_lib:format(Format, Data), Opts),
  {ok, Opts};
handle_event({info_report, GL, {Pid, Type, Data}} = _Event, Opts) ->
  Target = target(GL, Opts),
  info(Target, Pid, Type, format_report(Data), Opts),
  {ok, Opts};

handle_event(_Event, Opts) ->
  {ok, Opts}.

%% @private
%% @doc Handle {@link gen_event:call/3}.
handle_call(_Request, Opts) ->
  {ok, ok, Opts}.

%% @private
%% @doc Handle incoming messages.
handle_info(_Message, Opts) ->
  {ok, Opts}.

%% @private
%% @doc Handle code change.
code_change(_OldVsn, Opts, _Extra) ->
  {ok, Opts}.

%%%---------------------------------------------------------------------------

%% @doc Print error message.
%%   Function uses passed group leader to send characters to.
error(IODev, Pid, LogType, Line, Opts) ->
  Header = header(Pid, error, LogType, Opts),
  case Opts#opts.colour of
    true ->
      io:put_chars(IODev, [colour(red), Header, Line, colour_off(), "\n"]);
    false ->
      io:put_chars(IODev, [Header, Line, "\n"])
  end.

%% @doc Print warning message.
%%   Function uses passed group leader to send characters to.
warning(IODev, Pid, LogType, Line, Opts) ->
  Header = header(Pid, warning, LogType, Opts),
  case Opts#opts.colour of
    true ->
      io:put_chars(IODev, [colour(yellow), Header, Line, colour_off(), "\n"]);
    false ->
      io:put_chars(IODev, [Header, Line, "\n"])
  end.

%% @doc Print info message.
%%   Function uses passed group leader to send characters to.
info(IODev, Pid, LogType, Line, Opts) ->
  Header = header(Pid, info, LogType, Opts),
  io:put_chars(IODev, [Header, Line, "\n"]).

%% @doc Format header based on log level, type, PID of event origin and
%%   init options.
header(Pid, LogLevel, LogType, _Opts) ->
  io_lib:format("~p ~p[~p]: ", [Pid, LogLevel, LogType]).

%%%---------------------------------------------------------------------------

%% @doc Return address of target process based on options and group leader
%%   PID.

target(GroupLeader, #opts{iodev = group_leader} = _Opts) ->
  GroupLeader;
target(_GroupLeader, #opts{iodev = Target} = _Opts) ->
  Target.

%% @doc Format report as a single line.
%%   If it's a char list, it's left untouched. Otherwise, it's formatted as
%%   single-line Erlang term.
format_report(Report) ->
  case io_lib:char_list(Report) of
    true -> Report;
    % 16#ffffffff (4G) should be large enough for result not to wrap
    false -> io_lib:print(Report, 1, 16#ffffffff, -1)
  end.

%% @doc Return "set colour" xterm sequence.

-spec colour(red | green | yellow | blue | magenta | cyan | integer()) ->
  iolist().

colour(red    ) -> colour(31);
%colour(green  ) -> colour(32);
colour(yellow ) -> colour(33);
%colour(blue   ) -> colour(34);
%colour(magenta) -> colour(35);
%colour(cyan   ) -> colour(36);
colour(C) -> ["\e[", integer_to_list(C), ";1m"].

%% @doc Return "reset colour" xterm sequence.
colour_off() ->
  "\e[m".

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
