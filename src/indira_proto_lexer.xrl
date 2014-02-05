Definitions.

NUMBER = -?([1-9][0-9]*|0)(\.[0-9]+)?([eE][+-]?[0-9]+)?
STRING = "([^\\\"]|\\.)*"
SP = [\s\t\r\n]

%-----------------------------------------------------------

Rules.

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

%-----------------------------------------------------------

Erlang code.

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

%-----------------------------------------------------------
% vim:ft=erlang
