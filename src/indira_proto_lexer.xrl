Definitions.

NUMBER = -?([1-9][0-9]*|0)(\.[0-9]+)?([eE][+-]?[0-9]+)?
STRING = "([^\\\"]|\\.)*"
SP = [\s\t\r\n]

WORD = [a-zA-Z_][a-zA-Z0-9_.-]+

%-----------------------------------------------------------

Rules.

{WORD}={WORD}   : {token, {assign_string, TokenLine, eq_word(TokenChars)}}.
{WORD}={STRING} : {token, {assign_string, TokenLine, eq_str(TokenChars)}}.
{WORD}={NUMBER} : {token, {assign_number, TokenLine, eq_num(TokenChars)}}.

\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
,  : {token, {',', TokenLine}}.
\: : {token, {':', TokenLine}}.
{STRING} : {token, {string, TokenLine, to_str(TokenChars)}}.
{NUMBER} : {token, {number, TokenLine, to_num(TokenChars)}}.
true  : {token, {true,  TokenLine}}.
false : {token, {false, TokenLine}}.
null  : {token, {null,  TokenLine}}.
{SP}+ : skip_token.

{WORD} : {token, {word, TokenLine, list_to_binary(TokenChars)}}.

%-----------------------------------------------------------

Erlang code.

eq_word(String) ->
  {Name, Value} = split_on_equals(String),
  {list_to_binary(Name), list_to_binary(Value)}.

eq_str(String) ->
  {Name, Value} = split_on_equals(String),
  {list_to_binary(Name), to_str(Value)}.

eq_num(String) ->
  {Name, Value} = split_on_equals(String),
  {list_to_binary(Name), to_num(Value)}.

to_str(String) ->
  % TODO: better stringification
  % " \ / b f n r t \uXXXX
  list_to_binary(String).

to_num(String) ->
  case str_is_float(String) of
    true  -> list_to_float(String);
    false -> list_to_integer(String)
  end.

str_is_float("") ->
  false;
str_is_float([$e | _Str]) ->
  true;
str_is_float([$E | _Str]) ->
  true;
str_is_float([$. | _Str]) ->
  true;
str_is_float([_C | Str]) ->
  str_is_float(Str).

split_on_equals("=" ++ After) ->
  {"", After};
split_on_equals([C | Rest]) ->
  {BeforeTail, After} = split_on_equals(Rest),
  {[C | BeforeTail], After}.

%-----------------------------------------------------------
% vim:ft=erlang
