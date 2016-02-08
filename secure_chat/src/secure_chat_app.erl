-module(secure_chat_app).
-export([start/2, stop/1]).
-include("secure_chat.hrl").
-behaviour(application).


start(_StartType, _StartArgs) ->
	connect_nodes(),
	lager:start(),
	eredis_cluster:start(),

	setup_mnesia(),

	syn:start(),
	syn:init(),
    
    secure_chat_sup:start_link().

stop(_State) ->
    ok.

connect_nodes() ->
	{ok, Nodes} = application:get_env(nodes),
	[net_kernel:connect_node(Node) || Node <- Nodes, Node /= node()].

setup_mnesia() ->
	mnesia:create_schema([node()|nodes()]),
	mnesia:start(),

	Result = mnesia:create_table(message, [
		{attributes, record_info(fields, message)},
		{type, bag},
		{disc_copies, [node()|nodes()]}
	]),

	case Result of
		{atomic, ok} ->
			ok;
		{aborted, {already_exists, message}} ->
			add_table_to_current_node();
		Other ->
			{error, Other}
	end.

add_table_to_current_node() ->
	%mnesia:wait_for_tables([message], 5000),
	case mnesia:add_table_copy(message, node(), disc_copies) of
		{atomic, ok} ->
			ok;
		{aborted, {already_exists, message}} ->
			ok;
		Other ->
			{error, Other}
	end.