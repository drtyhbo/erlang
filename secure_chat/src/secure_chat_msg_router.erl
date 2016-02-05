-module(secure_chat_msg_router).
-export([add_user/2,
		route_msg/1]).
-include("secure_chat.hrl").

add_user(UserId, Pid) ->
	io:format("~p ~n", [{UserId, Pid}]),
	syn:register(UserId, Pid).

route_msg(Msg) ->
	case syn:find_by_key(Msg#message.to) of
	Pid ->
		secure_chat_user:receive_msg(Pid, Msg),
		ok;
	undefined ->
		offline
	end.