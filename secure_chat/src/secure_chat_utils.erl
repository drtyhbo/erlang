-module(secure_chat_utils).
-export([timestamp/0,
		flatten/1]).

-spec timestamp() -> integer().
timestamp() ->
	{Mega, Sec, _Micro} = os:timestamp(),
	(Mega * 1000000 + Sec).

flatten({struct, L}) ->
    flatten(L);
flatten([H | T]) ->
    [flatten(H) | flatten(T)];
flatten({K, V}) ->
    {K, flatten(V)};
flatten(Term) ->
    Term.
