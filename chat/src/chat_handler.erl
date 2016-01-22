-module(chat_handler).
-export([do_recv/1]).

do_recv(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, B} ->
			io:format("Recv ~p~n", [B]),
			do_recv(Socket);
		_ ->
			ok
	end.
	