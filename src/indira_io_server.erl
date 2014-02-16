%%%---------------------------------------------------------------------------
%%% @doc
%%%   I/O server suitable for being group leader.
%%%
%%%   This server only supports write operations. No options, no reading, only
%%%   writing. It's intended to send all printed data to {@link error_logger}.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_io_server).

-behaviour(gen_server).

%% supervision tree API
-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-record(state, {}).

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @doc Start I/O server process (without linking to it).
start() ->
  gen_server:start(?MODULE, [], []).

%% @doc Start I/O server process.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize event handler.
init(_Args) ->
  {ok, #state{}}.

%% @doc Clean up after event handler.
terminate(_Arg, _State) ->
  ok.

%% @doc Handle {@link gen_server:call/2}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}. % ignore unknown calls

%% @doc Handle {@link gen_server:cast/2}.
handle_cast(_Event, State) ->
  {noreply, State}. % ignore unknown casts

%% @doc Handle incoming messages.
handle_info({io_request, From, ReplyAs, Request} = _IORequest, State) ->
  Reply = handle_request(Request),
  send_reply(From, ReplyAs, Reply),
  {noreply, State};
handle_info(_Message, State) ->
  {noreply, State}. % ignore unknown messages

%% @doc Handle code change.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%---------------------------------------------------------------------------

%% @doc Send reply according to I/O protocol.
send_reply(From, ReplyAs, Reply) ->
  From ! {io_reply, ReplyAs, Reply}.

%% @doc Handle I/O requests and return result to be sent back.

%% write requests
handle_request({put_chars, _Enc, Characters}) ->
  % TODO: send it to `error_logger'
  io:put_chars(["\e[32;1m", Characters, "\e[m"]),
  ok;
handle_request({put_chars, Enc, Mod, Func, Args}) ->
  case apply(Mod, Func, Args) of
    Characters when is_list(Characters) or is_binary(Characters) ->
      handle_request({put_chars, Enc, Characters});
    _Any ->
      {error, badarg}
  end;

%% write requests (Erlang <R15B)
handle_request({put_chars, Characters}) ->
  handle_request({put_chars, latin1, Characters});
handle_request({put_chars, Mod, Func, Args}) ->
  handle_request({put_chars, latin1, Mod, Func, Args});

%% read requests -- no STDIN reading, always return EOF
handle_request({get_until, _Enc, _Prompt, _Mod, _Func, _ExtraArgs}) ->
  eof;
handle_request({get_chars, _Enc, _Prompt, _N}) ->
  eof;
handle_request({get_line, _Enc, _Prompt}) ->
  eof;

%% read requests (Erlang <R15B)
handle_request({get_until, Prompt, Mod, Func, ExtraArgs}) ->
  handle_request({get_until, latin1, Prompt, Mod, Func, ExtraArgs});
handle_request({get_chars, Prompt, N}) ->
  handle_request({get_chars, latin1, Prompt, N});
handle_request({get_line, Prompt}) ->
  handle_request({get_line, latin1, Prompt});

%% no options supported
handle_request({setopts, _Opts}) ->
  {error, enotsup};
handle_request(getopts) ->
  [];

%% multiple requests in a single message
handle_request({requests, Requests}) ->
  lists:foldl(
    fun
      (_Req, {error,_Reason} = Error) ->
        Error;
      (Req, _Result) ->
        handle_request(Req)
    end,
    ok, Requests
  );

%% querying geometry is not supported
handle_request({get_geometry, _Geometry}) ->
  {error, enotsup};

%% all the other requests are invalid
handle_request(_Any) ->
  {error, request}. 

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
