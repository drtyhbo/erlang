%%%-------------------------------------------------------------------
%% @doc secure_chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(secure_chat_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
	SupFlags = #{
		strategy => one_for_one,
		intensity => 3,
		period => 5000},
	ChildSpec = #{
		id => secure_chat_serv,
		start => {secure_chat_serv, start_link, [49165]},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [chat_serv]},
	{ok, {SupFlags, [ChildSpec]}}.