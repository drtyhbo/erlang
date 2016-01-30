-module(secure_chat_utils).
-export([timestamp/0]).

-spec timestamp() -> integer().
timestamp() ->
	{Mega, Sec, _Micro} = os:timestamp(),
	(Mega * 1000000 + Sec).