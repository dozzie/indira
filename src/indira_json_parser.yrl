
%%%---------------------------------------------------------------------------

Header
  "%%% @private"
.

Nonterminals
  value
  object array scalar
  key_value_list key_value
  value_list
.

Terminals
  '{' '}' '[' ']' ',' ':'
  string number true false null
.

Rootsymbol value.

%%%---------------------------------------------------------------------------
%%% JSON part

value -> object : '$1'.
value -> array  : '$1'.
value -> scalar : '$1'.

% NOTE: if there are duplicate keys, pairs order will be reversed as to their
% appearance (but JSON should not have duplicate keys anyway, so it shouldn't
% matter)
object -> '{' key_value_list '}' : lists:keysort(1, '$2').
object -> '{' '}'                : [{}].
array  -> '[' value_list ']' : lists:reverse('$2').
array  -> '[' ']'            : [].
scalar -> string : value('$1').
scalar -> number : value('$1').
scalar -> true   : true.
scalar -> false  : false.
scalar -> null   : null.

%% NOTE: reversed list order
key_value_list -> key_value_list ',' key_value : ['$3' | '$1'].
key_value_list -> key_value                    : ['$1'].

key_value -> string ':' value : {value('$1'), '$3'}.

%% NOTE: reversed list order
value_list -> value_list ',' value : ['$3' | '$1'].
value_list -> value                : ['$1'].

%%%---------------------------------------------------------------------------

Erlang code.

value({_TermName, _Line, Value}) ->
  Value.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang
