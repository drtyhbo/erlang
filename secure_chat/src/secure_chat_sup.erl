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
	SupFlags = #{
		strategy => one_for_all,
		intensity => 3,
		period => 5000},
	ListenerChildSpec = #{
		id => secure_chat_serv,
		start => {secure_chat_serv, start_link, [49165]},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [secure_chat_serv]},
	UserListChildSpec = #{
		id => secure_chat_user_list,
		start => {secure_chat_user_list, start_link, []},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [secure_chat_serv]},
	{ok, {SupFlags, [ListenerChildSpec, UserListChildSpec]}}.