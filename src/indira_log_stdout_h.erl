%%%---------------------------------------------------------------------------
%%% @doc
%%%   {@link error_logger} log handler (print to <i>STDOUT</i>).
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_log_stdout_h).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).

-record(opts, {
  colour = false
}).

%%%---------------------------------------------------------------------------
%%% gen_event callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize event handler.
init(Args) ->
  Colour = proplists:get_bool(colour, Args) orelse
           proplists:get_bool(color, Args),
  {ok, #opts{colour = Colour}}.

%% @doc Clean up after event handler.
terminate(_Arg, _Opts) ->
  ok.

%% @doc Handle incoming events.
%% @end
%%
%% message:
%%   Pid  :: pid() -- process sending an event
%%   GL   :: pid() -- group leader
%%   Type :: std_error | std_warning | std_info | term() -- type of `*_report'

handle_event({error, GL, {Pid, Format, Data}} = _Event, Opts) ->
  error(GL, Pid, message, io_lib:format(Format, Data), Opts),
  {ok, Opts};
handle_event({error_report, GL, {Pid, Type, Data}} = _Event, Opts) ->
  error(GL, Pid, Type, format_report(Data), Opts),
  {ok, Opts};

handle_event({warning_msg, GL, {Pid, Format, Data}} = _Event, Opts) ->
  warning(GL, Pid, message, io_lib:format(Format, Data), Opts),
  {ok, Opts};
handle_event({warning_report, GL, {Pid, Type, Data}} = _Event, Opts) ->
  warning(GL, Pid, Type, format_report(Data), Opts),
  {ok, Opts};

handle_event({info_msg, GL, {Pid, Format, Data}} = _Event, Opts) ->
  info(GL, Pid, message, io_lib:format(Format, Data), Opts),
  {ok, Opts};
handle_event({info_report, GL, {Pid, Type, Data}} = _Event, Opts) ->
  info(GL, Pid, Type, format_report(Data), Opts),
  {ok, Opts};

handle_event(_Event, Opts) ->
  {ok, Opts}.

%% @doc Handle {@link gen_event:call/3}.
handle_call(_Request, Opts) ->
  {ok, ok, Opts}.

%% @doc Handle incoming messages.
handle_info(_Message, Opts) ->
  {ok, Opts}.

%% @doc Handle code change.
code_change(_OldVsn, Opts, _Extra) ->
  {ok, Opts}.

%%%---------------------------------------------------------------------------

%% @doc Print error message.
%%   Function uses passed group leader to send characters to.
error(IODev, Pid, LogType, Line, Opts) ->
  Header = header(Pid, info, LogType, Opts),
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
