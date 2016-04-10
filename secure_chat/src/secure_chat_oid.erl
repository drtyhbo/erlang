-module(secure_chat_oid).
-export([oid_to_str/1,
         hexstr_to_bin/1,
         str_to_oid/1]).

oid_to_str(Oid) ->
	{BinaryList} = Oid,
	list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(BinaryList)])).

hexstr_to_bin(S) ->
	hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
	list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
	{ok, [V], []} = io_lib:fread("~16u", [X,Y]),
	hexstr_to_bin(T, [V | Acc]).

str_to_oid(S) ->
	{hexstr_to_bin(binary_to_list(S))}.
