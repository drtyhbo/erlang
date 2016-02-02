-module(secure_chat_msg_router).
-export([add_user/3,
		remove_user/2,
		route_msg/2]).
-include("secure_chat.hrl").

add_user(UserList, UserId, Pid) ->
	io:format("~p ~n", [{UserId, Pid}]),
	ets:insert(UserList, {UserId, Pid}).

remove_user(UserList, UserId) ->
	ets:delete(UserList, UserId).

route_msg(UserList, Msg) ->
	case ets:lookup(UserList, Msg#message.to) of
	[{_, Pid}] ->
		secure_chat_user:receive_msg(Pid, Msg),
		ok;
	_ ->
		offline
	end.