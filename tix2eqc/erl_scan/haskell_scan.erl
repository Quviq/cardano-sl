-module(haskell_scan).
-export([string/2]).

string(String, Pos) ->
    string(String, Pos, []).

string([], Pos, Tokens) ->
    {ok, lists:reverse(Tokens), Pos};
string([Char|Chars], {X, Y}, Tokens) ->
    Token = {var, {X, Y}, list_to_atom([Char])},
    string(Chars, next(Char, {X, Y}), [Token|Tokens]).

next($\n, {X, _}) ->
    {X+1, 1};
next(_, {X, Y}) ->
    {X, Y+1}.
