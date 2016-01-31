-module(secure_chat_msg_router).
-export([add_user/3,
		remove_user/2,
		route_msg/2]).
-include("secure_chat.hrl").

add_user(UserList, Username, Pid) ->
	ets:insert(UserList, {Username, Pid}).

remove_user(UserList, Username) ->
	ets:delete(UserList, Username).

route_msg(UserList, Msg) ->
	case ets:lookup(UserList, Msg#message.to) of
	[{_, Pid}] ->
		secure_chat_user:receive_msg(Pid, Msg),
		ok;
	_ ->
		offline
	end.