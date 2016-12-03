%%%---------------------------------------------------------------------------
%%% @doc
%%%   Erlang term (jsx-style,
%%%   [http://www.erlang.org/eeps/eep-0018.html EEP-0018]) to JSON serializer
%%%   and deserializer.
%%%
%%%   For deserialized JSON hashes, they are compatible with functions from
%%%   {@link orddict} module.
%%%
%%%   Note that Erlang strings are generally recognized as arrays of integers.
%%%   Note also that tuples alone are not allowed. They can only compose
%%%   proplists that will be serialized to a JSON object.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_json).

-export([encode/1, decode/1]).
-export([format_error/1]).

-export_type([json_string/0, struct/0]).
-export_type([jhash/0, jarray/0, jscalar/0]).

%%%---------------------------------------------------------------------------

-type json_string() :: string() | binary().
%% Input formatted as JSON.

-type struct() :: jhash() | jarray() | jscalar().

-type jhash() :: [{ jstring(), struct() }, ...] | [{}].
%% Hash node. Literal `[{}]' represents an empty hash. If the hash is
%% non-empty, it's a non-empty, ordered list of 2-tuples, compatible with
%% {@link orddict}.

-type jarray() :: [struct()].

-type jscalar() :: jstring() | number() | null | true | false.

-type jstring() :: binary() | atom().
%% String to be serialized. May be an atom, except for `true', `false', and
%% `null'.

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%% @doc Encode Erlang term (jsx-style) to JSON.
%%   Function returns `{error,badarg}' if data couldn't be encoded.
%%
%%   Note that strings in Erlang are lists of integers. This module doesn't
%%   guess what's in the list, so you need to pass binary to get JSON string.

-spec encode(struct()) ->
  {ok, iolist()} | {error, badarg}.

encode(Struct) ->
  try
    {ok, encode_value(Struct)}
  catch
    error:{badmatch,_Any} ->
      {error, badarg};
    error:{case_clause,_Any} ->
      {error, badarg};
    %error:if_clause -> % unused in this module
    %  {error, badarg};
    error:function_clause ->
      {error, badarg}
  end.

%% @doc Decode a string containing JSON to a (jsx-style) term.

-spec decode(json_string()) ->
  {ok, struct()} | {error, badarg}.

decode(Line) when is_binary(Line) ->
  decode(binary_to_list(Line));

decode(Line) when is_list(Line) ->
  case indira_json_lexer:string(Line) of
    {ok, Tokens, _EndLine} ->
      case indira_json_parser:parse(Tokens) of
        {ok, Result} ->
          {ok, Result};
        {error, {_LineNumber, _ParserModule, _Message}} ->
          {error, badarg}
      end;
    {error, {_LineNumber, _LexerModule, _Message}, _} ->
      {error, badarg}
  end.

%% @doc Convert a `Reason' from error tuple into usable error message.

-spec format_error(term()) ->
  string().

format_error(badarg = _Reason) -> "invalid argument";
format_error(_Reason) -> "unknown JSON error".

%%%---------------------------------------------------------------------------
%%% serializer {{{

%% @doc Encode any value.

-spec encode_value(struct()) ->
  iolist().

encode_value([{}] = _Struct) ->
  "{}"; % short circuit for empty objects
encode_value([{_,_} | _] = Struct) ->
  [${, encode_sequence(Struct, fun encode_pair/1), $}];
encode_value([] = _Struct) ->
  "[]"; % short circuit for empty arrays
encode_value([_ | _] = Struct) ->
  [$[, encode_sequence(Struct, fun encode_value/1), $]];
encode_value(null = _Struct) ->
  "null";
encode_value(true = _Struct) ->
  "true";
encode_value(false = _Struct) ->
  "false";
encode_value(Struct) when is_binary(Struct); is_atom(Struct) ->
  encode_string(Struct);
encode_value(Struct) when is_number(Struct) ->
  encode_number(Struct).

%%----------------------------------------------------------
%% serializing sequences (lists and hashes) {{{

%% @doc Encode sequence of pairs|values.
%%   Function expects a non-empty list of elements.
%%
%%   Combined with {@link encode_pair/1} serializes hashes, combined with
%%   {@link encode_value/1} serializes lists.

-spec encode_sequence([term()], fun((term()) -> iolist())) ->
  iolist().

encode_sequence([E] = _Sequence, EncodeElement) ->
  EncodeElement(E);
encode_sequence([E | Rest] = _Sequence, EncodeElement) ->
  [EncodeElement(E), "," | encode_sequence(Rest, EncodeElement)].

%% @doc Encode single key/value pair in hash.
%%
%% @see encode_sequence/2

-spec encode_pair({Key :: string() | binary() | atom(), Value :: struct()}) ->
  iolist().

encode_pair({K, V} = _Pair) ->
  [encode_string(K), ":" | encode_value(V)].

%% }}}
%%----------------------------------------------------------
%% serializing scalars {{{

%% @doc Encode string.

-spec encode_string(string() | binary() | atom()) ->
  iolist().

encode_string(String) when is_list(String) ->
  [$", string_quote(String), $"];
encode_string(String) when is_atom(String) ->
  encode_string(atom_to_list(String));
encode_string(String) when is_binary(String) ->
  encode_string(binary_to_list(String)).

%% @doc Quote all characters that can't be expressed literally in JSON string.

-spec string_quote(string()) ->
  iolist().

string_quote("" = _String) -> "";
string_quote([C | Rest]) when C == $"  -> "\\\"" ++ string_quote(Rest);
string_quote([C | Rest]) when C == $\\ -> "\\\\" ++ string_quote(Rest);
string_quote([C | Rest]) when C == $\b -> "\\b" ++ string_quote(Rest);
string_quote([C | Rest]) when C == $\f -> "\\f" ++ string_quote(Rest);
string_quote([C | Rest]) when C == $\n -> "\\n" ++ string_quote(Rest);
string_quote([C | Rest]) when C == $\r -> "\\r" ++ string_quote(Rest);
string_quote([C | Rest]) when C == $\t -> "\\t" ++ string_quote(Rest);
string_quote([C | Rest]) when C < 32 -> encode_unicode(C) ++ string_quote(Rest);
string_quote([C | Rest]) -> [C | string_quote(Rest)].

%% @doc Encode single unicode character as a `\uXXXX' sequence.

-spec encode_unicode(char()) ->
  string().

encode_unicode(Char) ->
  case erlang:integer_to_list(Char, 16) of
    [_]       = S -> "\\u000" ++ S;
    [_,_]     = S -> "\\u00"  ++ S;
    [_,_,_]   = S -> "\\u0"   ++ S;
    [_,_,_,_] = S -> "\\u"    ++ S
  end.

%% @doc Encode number (integer or float) as a string.

-spec encode_number(number()) ->
  string().

encode_number(N) when is_float(N) ->
  float_to_list(N);
encode_number(N) when is_integer(N) ->
  integer_to_list(N).

%% }}}
%%----------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
