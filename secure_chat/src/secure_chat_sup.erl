-module(secure_chat_sup).
-behaviour(supervisor).
-export([init/1, start_link/0, stop/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    supervisor:terminate_child(?MODULE, secure_chat_serv),
    supervisor:terminate_child(?MODULE, secure_user_chat_list),
    supervisor:delete_child(?MODULE, secure_chat_serv),
    supervisor:delete_child(?MODULE, secure_user_chat_list).

init([]) ->
	{ok, RedisConnection} = eredis:start_link(),
	SupFlags = #{
		strategy => one_for_all,
		intensity => 3,
		period => 5000},
	ListenerChildSpec = #{
		id => secure_chat_serv,
		start => {secure_chat_serv, start_link, [49165, RedisConnection]},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [secure_chat_serv]},
	MsgRouterChildSpec = #{
		id => secure_chat_msg_router,
		start => {secure_chat_msg_router, start_link, []},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [secure_chat_msg_router]},
	MsgStoreChildSpec = #{
		id => secure_chat_msg_store,
		start => {secure_chat_msg_store, start_link, [RedisConnection]},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [secure_chat_msg_store]},
	{ok, {SupFlags, [ListenerChildSpec, MsgRouterChildSpec, MsgStoreChildSpec]}}.