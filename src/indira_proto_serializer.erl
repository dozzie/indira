%%%---------------------------------------------------------------------------
%%% @doc
%%%   Erlang term (jsx-style) to JSON serializer.
%%% @end
%%%---------------------------------------------------------------------------

-module(indira_proto_serializer).

-export([encode/1]).

%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%% @doc Encode Erlang term (jsx-style) to JSON.
%%   Function returns `{error,badarg}' if data couldn't be encoded.
%%
%%   Note that strings in Erlang are lists of integers. This module doesn't
%%   guess what's in the list, so you need to pass binary to get JSON string.
%%
%% @spec encode(term()) ->
%%   {ok, iolist()} | {error, badarg}
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

%%%---------------------------------------------------------------------------
%%% internal functions
%%%---------------------------------------------------------------------------

%% @doc Encode any value.
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

%% @doc Encode sequence of pairs|values.
%%   Function expects a non-empty list of elements.
encode_sequence([E] = _Sequence, EncodeElement) ->
  EncodeElement(E);
encode_sequence([E | Rest] = _Sequence, EncodeElement) ->
  [EncodeElement(E), "," | encode_sequence(Rest, EncodeElement)].

%% @doc Encode single key/value pair in object.
encode_pair({K, V} = _Pair) ->
  [encode_string(K), ":" | encode_value(V)].

%% @doc Encode string.
encode_string(String) when is_list(String) ->
  [$", string_quote(String), $"];
encode_string(String) when is_atom(String) ->
  encode_string(atom_to_list(String));
encode_string(String) when is_binary(String) ->
  encode_string(binary_to_list(String)).

%% @doc Quote all characters that can't be expressed literally in JSON string.
%% @TODO Unicode in lists.
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
encode_unicode(Char) ->
  case erlang:integer_to_list(Char, 16) of
    [_]       = S -> "\\u000" ++ S;
    [_,_]     = S -> "\\u00"  ++ S;
    [_,_,_]   = S -> "\\u0"   ++ S;
    [_,_,_,_] = S -> "\\u"    ++ S
  end.

%% @doc Encode number (integer or float) as a string.
encode_number(N) when is_float(N) ->
  float_to_list(N);
encode_number(N) when is_integer(N) ->
  integer_to_list(N).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
