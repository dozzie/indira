%%%---------------------------------------------------------------------------
%%% @doc
%%%   Indira interface for direct command line use.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira).

%% public interface
-export([start/0, cookie_file/1]).

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Start Indira application.
%%
%%   This function is intended to be used from command line:
%% ```
%% $ erl \
%%     -indira listen '[{indira_tcp, {any,5500}}]' \
%%     -indira command '{some_module, []}' \
%%     -s indira \
%%     other args ...
%% '''

-spec start() ->
  ok | {error, term()}.

start() ->
  application:start(indira, permanent).

%% @doc Set Erlang cookie to the content of specified file.
%%
%%   `Args' should be a list of filenames with exactly one element.
%%
%%   If the file contains multiple lines, only the first one is taken as
%%   a cookie.
%%
%%   This function makes it easier to connect to an already running node,
%%   configured by Indira with cookie set to `{file, CookieFile}'. Command line use:
%```
%erl -sname shell -run indira cookie_file /etc/daemon/cookie.txt
%'''

-spec cookie_file(Args :: [file:filename()]) ->
  ok.

cookie_file([CookieFile]) ->
  {ok, Cookie} = indira_dist_erl:read_cookie(CookieFile),
  erlang:set_cookie(node(), Cookie),
  ok.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
