%%%---------------------------------------------------------------------------
%%% @doc
%%%   Syslog connection handles and message formatting.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_syslog).

%% message formatting API
-export([format/4, format/5]).
-export([encode/2, decode/1, facility/1, priority/1]).
%% syslog connection handling
-export([open_local/1, open_remote/2]).
-export([send/2]).
-export([controlling_process/2]).
-export([close/1]).
%% error formatting
-export([format_error/1]).

-export_type([facility/0, priority/0, ident/0, connection/0]).

%%%---------------------------------------------------------------------------
%%% data types {{{

-type facility() ::
    kern | user | daemon | cron | syslog | auth | authpriv
  | mail | ftp | lpr | news | uucp
  | local0 | local1 | local2 | local3 | local4 | local5 | local6 | local7.
%% Meaning of the facilities:
%% <ul>
%%   <li><i>kern</i> -- kernel messages</li>
%%   <li><i>user</i> -- random user-level messages</li>
%%   <li><i>daemon</i> -- system daemons</li>
%%   <li><i>cron</i> -- clock daemon</li>
%%   <li><i>syslog</i> -- messages generated internally by syslogd</li>
%%   <li><i>auth</i> -- security/authorization messages</li>
%%   <li><i>authpriv</i> -- security/authorization messages (private)</li>
%%   <li><i>mail</i> -- mail system</li>
%%   <li><i>ftp</i> -- FTP daemon</li>
%%   <li><i>lpr</i> -- line printer subsystem</li>
%%   <li><i>news</i> -- network news subsystem</li>
%%   <li><i>uucp</i> -- UUCP subsystem</li>
%%   <li><i>local0</i>..<i>local7</i> -- reserved for local use</li>
%% </ul>

-type priority() ::
  emerg | alert | crit | err | warning | notice | info | debug.
%% Meaning of the priorities:
%% <ul>
%%   <li><i>emerg</i> -- system is unusable</li>
%%   <li><i>alert</i> -- action must be taken immediately</li>
%%   <li><i>crit</i> -- critical conditions</li>
%%   <li><i>err</i> -- error conditions</li>
%%   <li><i>warning</i> -- warning conditions</li>
%%   <li><i>notice</i> -- normal but significant condition</li>
%%   <li><i>info</i> -- informational</li>
%%   <li><i>debug</i> -- debug-level messages</li>
%% </ul>

-type ident() :: string() | atom().
%% Identification of current process (daemon name).

-type hostname() :: string().
%% Host being the source of a message.

-type connection() :: term().

%%% }}}
%%%---------------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% message formatting API
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% message formatting {{{

%% @doc Format message for sending to syslog.
%%
%%   Returned data doesn't contain trailing newline. Note that it also doesn't
%%   contain hostname, as this is added by syslog daemon.

-spec format(facility(), priority(), ident(), iolist()) ->
  iolist().

format(Facility, Priority, Ident, Message) ->
  % <158>May  4 04:50:21 aaa[12345]: foo
  io_lib:format("<~B>~s ~s[~s]: ~s",
                [encode(Facility, Priority), syslog_time(), Ident, os:getpid(),
                  Message]).

%% @doc Format message for sending to syslog.
%%
%%   Returned data doesn't contain trailing newline.

-spec format(facility(), hostname(), priority(), ident(), iolist()) ->
  iolist().

format(Facility, Hostname, Priority, Ident, Message) ->
  % <158>May  4 04:50:21 host01 aaa[12345]: foo
  io_lib:format("<~B>~s ~s ~s[~s]: ~s",
                [encode(Facility, Priority), syslog_time(),
                  Hostname, Ident, os:getpid(),
                  Message]).

%% }}}
%%----------------------------------------------------------
%% time formatting {{{

%% @doc Syslog-formatted current time.

-spec syslog_time() ->
  iolist().

syslog_time() ->
  syslog_time(os:timestamp()).

%% @doc Syslog-formatted time.

-spec syslog_time(erlang:timestamp()) ->
  iolist().

syslog_time(Timestamp) ->
  {{_Year,Month,Day},{H,M,S}} = calendar:now_to_local_time(Timestamp),
  io_lib:format("~s ~2B ~2..0B:~2..0B:~2..0B", [month(Month), Day, H, M, S]).

%% @doc Convert month number to abbreviated English name.

-spec month(MonthNumber :: 1..12) ->
  string().

month(1)  -> "Jan";
month(2)  -> "Feb";
month(3)  -> "Mar";
month(4)  -> "Apr";
month(5)  -> "May";
month(6)  -> "Jun";
month(7)  -> "Jul";
month(8)  -> "Aug";
month(9)  -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% }}}
%%----------------------------------------------------------
%% facility and priority {{{

%% @doc Decode syslog-encoded facility and priority to atoms.

-spec decode(non_neg_integer()) ->
  {facility(), priority()}.

decode(Combined) when is_integer(Combined) ->
  Facility = Combined div 8,
  Priority = Combined rem 8,
  {facility(Facility), priority(Priority)}.

%% @doc Encode facility and priority

-spec encode(facility(), priority()) ->
  integer().

encode(Facility, Priority) when is_atom(Facility), is_atom(Priority) ->
  facility(Facility) * 8 + priority(Priority).

%% @doc Convert atom describing priority to number or the reverse.
%%
%% @spec priority(integer() | priority()) ->
%%   priority() | integer()

-spec priority(integer()) -> priority();
              (priority()) -> integer().

priority(0) -> emerg;
priority(1) -> alert;
priority(2) -> crit;
priority(3) -> err;
priority(4) -> warning;
priority(5) -> notice;
priority(6) -> info;
priority(7) -> debug;
priority(emerg)   -> 0; % system is unusable
priority(alert)   -> 1; % action must be taken immediately
priority(crit)    -> 2; % critical conditions
priority(err)     -> 3; % error conditions
priority(warning) -> 4; % warning conditions
priority(notice)  -> 5; % normal but significant condition
priority(info)    -> 6; % informational
priority(debug)   -> 7. % debug-level messages

%% @doc Convert atom describing facility to number or the reverse.
%%
%% @spec facility(integer() | facility()) ->
%%   facility() | integer()

-spec facility(integer()) -> facility();
              (facility()) -> integer().

facility(0)  -> kern;
facility(1)  -> user;
facility(2)  -> mail;
facility(3)  -> daemon;
facility(4)  -> auth;
facility(5)  -> syslog;
facility(6)  -> lpr;
facility(7)  -> news;
facility(8)  -> uucp;
facility(9)  -> cron;
facility(10) -> authpriv;
facility(11) -> ftp;
facility(16) -> local0;
facility(17) -> local1;
facility(18) -> local2;
facility(19) -> local3;
facility(20) -> local4;
facility(21) -> local5;
facility(22) -> local6;
facility(23) -> local7;
facility(kern)     ->  0; % kernel messages
facility(user)     ->  1; % random user-level messages
facility(mail)     ->  2; % mail system
facility(daemon)   ->  3; % system daemons
facility(auth)     ->  4; % security/authorization messages
facility(syslog)   ->  5; % messages generated internally by syslogd
facility(lpr)      ->  6; % line printer subsystem
facility(news)     ->  7; % network news subsystem
facility(uucp)     ->  8; % UUCP subsystem
facility(cron)     ->  9; % clock daemon
facility(authpriv) -> 10; % security/authorization messages (private)
facility(ftp)      -> 11; % ftp daemon
facility(local0)   -> 16; % reserved for local use
facility(local1)   -> 17; % reserved for local use
facility(local2)   -> 18; % reserved for local use
facility(local3)   -> 19; % reserved for local use
facility(local4)   -> 20; % reserved for local use
facility(local5)   -> 21; % reserved for local use
facility(local6)   -> 22; % reserved for local use
facility(local7)   -> 23. % reserved for local use

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% syslog connection handling
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% opening connection {{{

%% @doc Open connection to local syslog.

-spec open_local(string()) ->
  {ok, connection()} | {error, term()}.

open_local(SocketPath) ->
  % skip loading port driver if Indira is already running
  case whereis(indira_af_unix_manager) of
    undefined -> indira_af_unix:load_port_driver();
    Pid when is_pid(Pid) -> ok
  end,
  case indira_af_unix:connect(SocketPath, [{active, false}]) of
    {ok, Socket}    -> {ok, {unix, Socket}};
    {error, Reason} -> {error, Reason}
  end.

%% @doc Open connection to remote syslog (UDP).

-spec open_remote(inet:hostname() | inet:ip_address(),
                  inet:port_number() | default) ->
  {ok, connection()} | {error, term()}.

open_remote(Host, default = _Port) ->
  open_remote(Host, 514);

open_remote(Host, Port) when is_atom(Host); is_list(Host) ->
  case inet:getaddr(Host, inet) of
    {ok, Addr}      -> open_remote(Addr, Port);
    {error, Reason} -> {error, Reason}
  end;

open_remote(Host, Port) when is_tuple(Host) ->
  % would be very strange if there was a problem with this
  {ok, Socket} = gen_udp:open(0),
  Connection = {udp, Socket, {Host, Port}},
  {ok, Connection}.

%% }}}
%%----------------------------------------------------------
%% sending a message {{{

%% @doc Send a line to syslog.

-spec send(connection(), iolist()) ->
  ok | {error, term()}.

send({unix, Socket} = _Syslog, Message) ->
  indira_af_unix:send(Socket, [Message, "\n"]);

send({udp, Socket, {Host, Port}} = _Syslog, Message) ->
  gen_udp:send(Socket, Host, Port, Message).

%% }}}
%%----------------------------------------------------------
%% changing the connection owner {{{

%% @doc Set the connection owner.

-spec controlling_process(connection(), pid()) ->
  ok | {error, not_owner | badarg}.

controlling_process({unix, Socket} = _Syslog, Pid) ->
  indira_af_unix:controlling_process(Socket, Pid);

controlling_process({udp, Socket, {_Host, _Port}} = _Syslog, Pid) ->
  gen_udp:controlling_process(Socket, Pid).

%% }}}
%%----------------------------------------------------------
%% closing connection {{{

%% @doc Close a connection to syslog.

-spec close(connection()) ->
  ok.

close({unix, Socket} = _Syslog) ->
  indira_af_unix:close(Socket),
  indira_af_unix:unload_port_driver(),
  ok;

close({udp, Socket, {_Host, _Port}} = _Syslog) ->
  gen_udp:close(Socket),
  ok.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------

%% @doc Convert a `Reason' from error tuple into usable error message.

-spec format_error(term()) ->
  string().

format_error(Reason) ->
  % `gen_udp' and `indira_af_unix' have pretty much the same errors, except
  % for `gen_udp:send()', which can return `{error,not_owner}', but even this
  % is covered by `indira_af_unix'
  indira_af_unix:format_error(Reason).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
