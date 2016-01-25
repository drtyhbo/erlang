%%%-------------------------------------------------------------------
%% @doc secure_chat public API
%% @end
%%%-------------------------------------------------------------------

-module(secure_chat_app).
-behaviour(application).
-export([start/2, stop/1, shutdown/0]).

start(_StartType, _StartArgs) ->
	setup(),
    secure_chat_sup:start_link().

stop(_State) ->
    ok.

shutdown() ->
	secure_chat_sup:stop(),
	application:stop(chat).

setup() ->
	lager:start().