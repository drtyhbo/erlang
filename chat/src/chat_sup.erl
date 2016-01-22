-module(chat_sup).
-export([start_link/0, stop/0, init/1]).
-behavior(supervisor).

start_link() ->
	supervisor:start_link({local, chat}, ?MODULE, []).

stop() ->
    supervisor:terminate_child(chat, chat_serv),
    supervisor:delete_child(chat, chat_serv).

init([]) ->
	SupFlags = #{
		strategy => one_for_one,
		intensity => 3,
		period => 5000},
	ChildSpec = #{
		id => chat_serv,
		start => {chat_serv, start_link, [49165]},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [chat_serv]},
	{ok, {SupFlags, [ChildSpec]}}.