-module(secure_chat_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).


-define(CHILD(Module, Params), #{
		id => Module,
		start => {Module, start_link, Params},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [Module]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	SupFlags = #{
		strategy => one_for_all,
		intensity => 3,
		period => 5000},
	Children = [
		?CHILD(secure_chat_serv, [49165]),
		?CHILD(secure_chat_msg_store, [])],
	{ok, {SupFlags, Children}}.