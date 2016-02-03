%%%-------------------------------------------------------------------
%% @doc secure_chat public API
%% @end
%%%-------------------------------------------------------------------

-module(secure_chat_app).
-export([start/2, stop/1, shutdown/0]).
-include("secure_chat.hrl").
-behaviour(application).

start(_StartType, _StartArgs) ->
	lager:start(),
	eredis_cluster:start(),
	eredis_cluster:connect([
        {"127.0.0.1", 30001},
        {"127.0.0.1", 30002}]),
	setup_mnesia(),
    secure_chat_sup:start_link().

stop(_State) ->
    ok.

shutdown() ->
	secure_chat_sup:stop(),
	application:stop(chat).

setup_mnesia() ->
	ok = application:start(mnesia),
	mnesia:create_schema([node()]),
	{atomic, ok} = mnesia:create_table(message, [
		{attributes, record_info(fields, message)},
		{type, bag}]),
	mnesia:wait_for_tables([message], 5000).