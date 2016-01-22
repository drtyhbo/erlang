-module(chat).
-behavior(application).
-export([start/2, stop/1, shutdown/0]).

start(normal, _Args) ->
	chat_sup:start_link().

shutdown() ->
	chat_sup:stop(),
	application:stop(chat).

stop(_State) ->
	ok.