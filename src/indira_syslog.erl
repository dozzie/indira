%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira syslog connection handles and message formatting.
%%%
%%% @TODO Make the port survive syslog restarts.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_syslog).

%% message formatting API
-export([format/4]).
-export([encode/2, decode/1, facility/1, priority/1]).
%% syslog connection handling
-export([open_local/1, open_remote/2]).
-export([send/2]).
-export([close/1]).

%%%---------------------------------------------------------------------------

%% for code:lib_dir/2
-define(APP_NAME, indira).

%%%---------------------------------------------------------------------------
%%% data types {{{

%% @type facility() =
%%     kern | user | daemon | cron | syslog | auth | authpriv
%%   | mail | ftp | lpr | news | uucp
%%   | local0 | local1 | local2 | local3 | local4 | local5 | local6 | local7.
%%   Meaning of facilities:
%%   <ul>
%%     <li><i>kern</i> -- kernel messages</li>
%%     <li><i>user</i> -- random user-level messages</li>
%%     <li><i>daemon</i> -- system daemons</li>
%%     <li><i>cron</i> -- clock daemon</li>
%%     <li><i>syslog</i> -- messages generated internally by syslogd</li>
%%     <li><i>auth</i> -- security/authorization messages</li>
%%     <li><i>authpriv</i> -- security/authorization messages (private)</li>
%%     <li><i>mail</i> -- mail system</li>
%%     <li><i>ftp</i> -- FTP daemon</li>
%%     <li><i>lpr</i> -- line printer subsystem</li>
%%     <li><i>news</i> -- network news subsystem</li>
%%     <li><i>uucp</i> -- UUCP subsystem</li>
%%     <li><i>local0</i>..<i>local7</i> -- reserved for local use</li>
%%   </ul>
-type facility() ::
    kern | user | daemon | cron | syslog | auth | authpriv
  | mail | ftp | lpr | news | uucp
  | local0 | local1 | local2 | local3 | local4 | local5 | local6 | local7.

%% @type priority() =
%%   emerg | alert | crit | err | warning | notice | info | debug.
%%   Meaning of priorities:
%%   <ul>
%%     <li><i>emerg</i> -- system is unusable</li>
%%     <li><i>alert</i> -- action must be taken immediately</li>
%%     <li><i>crit</i> -- critical conditions</li>
%%     <li><i>err</i> -- error conditions</li>
%%     <li><i>warning</i> -- warning conditions</li>
%%     <li><i>notice</i> -- normal but significant condition</li>
%%     <li><i>info</i> -- informational</li>
%%     <li><i>debug</i> -- debug-level messages</li>
%%   </ul>

-type priority() ::
  emerg | alert | crit | err | warning | notice | info | debug.

%% @type ident() = string() | atom().
%%   Identification of current process (daemon name).

-type ident() :: string() | atom().

%% @type connection() = term().

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
%%
%% @spec format(facility(), priority(), ident(), iolist()) ->
%%   iolist()

-spec format(facility(), priority(), ident(), iolist()) ->
  iolist().

format(Facility, Priority, Ident, Message) ->
  % <158>May  4 04:50:21 aaa[12345]: foo
  io_lib:format("<~B>~s ~s[~s]: ~s",
                [encode(Facility, Priority), syslog_time(), Ident, os:getpid(),
                  Message]).

%% }}}
%%----------------------------------------------------------
%% time formatting {{{

%% @doc Syslog-formatted current time.
%%
%% @spec syslog_time() ->
%%   iolist()

syslog_time() ->
  syslog_time(os:timestamp()).

%% @doc Syslog-formatted time.
%%
%% @spec syslog_time({MegaSecs :: integer(), Secs :: integer(),
%%                     MicroSecs :: integer()}) ->
%%   iolist()

syslog_time(Timestamp) ->
  {{_Year,Month,Day},{H,M,S}} = calendar:now_to_local_time(Timestamp),
  io_lib:format("~s ~2B ~2..0B:~2..0B:~2..0B", [month(Month), Day, H, M, S]).

%% @doc Convert month number to abbreviated English name.

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
%%
%% @spec decode(integer()) ->
%%   {facility(), priority()}

-spec decode(integer()) ->
  {facility(), priority()}.

decode(Combined) when is_integer(Combined) ->
  Facility = Combined div 8,
  Priority = Combined rem 8,
  {facility(Facility), priority(Priority)}.

%% @doc Encode facility and priority
%%
%% @spec encode(facility(), priority()) ->
%%   integer()

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
%%
%% @spec open_local(string()) ->
%%   connection()

-spec open_local(string()) ->
  connection().

open_local(SocketPath) ->
  {ok, Socket} = indira_af_unix:connect(SocketPath, [{active, false}]),
  {unix, Socket}.

%% @doc Open connection to remote syslog (UDP).
%%
%% @spec open_remote(inet:hostname() | inet:ip_address(),
%%                          integer() | default) ->
%%   connection()

-spec open_remote(inet:hostname() | inet:ip_address(),
                         integer() | default) ->
  connection().

open_remote(Host, default = _Port) ->
  open_remote(Host, 514);

open_remote(Host, Port) when is_atom(Host); is_list(Host) ->
  {ok, Addr} = inet:getaddr(Host, inet),
  open_remote(Addr, Port);

open_remote(Host, Port) when is_tuple(Host) ->
  {ok, Socket} = gen_udp:open(0),
  {udp, Socket, {Host, Port}}.

%% }}}
%%----------------------------------------------------------
%% sending a message {{{

%% @doc Send a line to syslog.
%%
%% @spec send(connection(), iolist()) ->
%%   ok

-spec send(connection(), iolist()) ->
  ok.

send({unix, Socket} = _Syslog, Message) ->
  indira_af_unix:send(Socket, [Message, "\n"]),
  ok;

send({udp, Socket, {Host, Port}} = _Syslog, Message) ->
  gen_udp:send(Socket, Host, Port, Message),
  ok.

%% }}}
%%----------------------------------------------------------
%% closing connection {{{

%% @doc Close a connection to syslog.
%%
%% @spec close(connection()) ->
%%   ok

-spec close(connection()) ->
  ok.

close({unix, Socket} = _Syslog) ->
  indira_af_unix:close(Socket),
  ok;

close({udp, Socket, {_Host, _Port}} = _Syslog) ->
  gen_udp:close(Socket),
  ok.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
