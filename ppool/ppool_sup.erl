-module(ppool_sup).
-export([start_link/3, init/1]).
-behavior(supervisor).

start_link(Name, Limit, MFA) ->
	supervisor:start_link(?MODULE, {Name, Limit, MFA}).

init({Name, Limit, MFA}) ->
	SupFlags = sup_flags#{
		strategy=one_for_all,
		intensity=1,
		period=3600}
	ChildSpec = child_spec#{
		id=serv,
		start={ppool_serv, start_link, [Name, Limit, self(), MFA]},
		restart=permanent,
		shutdown=5000,
		type=worker,
		modules=[ppool_serv]},
	{ok, {SupFlags, [ChildSpec]}}.