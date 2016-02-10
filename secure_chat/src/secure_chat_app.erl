-module(secure_chat_app).
-export([start/2, stop/1]).
-include("secure_chat.hrl").
-behaviour(application).


start(_StartType, _StartArgs) ->
	connect_nodes(),
	lager:start(),
	eredis_cluster:start(),

	ok = setup_mnesia(),

	syn:start(),
	syn:init(),
    
    secure_chat_sup:start_link().

stop(_State) ->
    ok.

connect_nodes() ->
	{ok, Nodes} = application:get_env(nodes),
	[net_kernel:connect_node(Node) || Node <- Nodes, Node /= node()].

setup_mnesia() ->
	mnesia:start(),
    mnesia:change_config(extra_db_nodes, [node() | nodes()]),
	Result = mnesia:create_table(message, [
		{attributes, record_info(fields, message)},
		{type, bag},
		{disc_copies, [node()]}
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
	mnesia:wait_for_tables([message], 10000),
	case mnesia:change_table_copy_type(schema, node(), disc_copies) of
		{atomic, ok} ->
			case mnesia:add_table_copy(message, node(), disc_copies) of
				{atomic, ok} ->
					ok;
				{aborted, {already_exists, message}} ->
					ok;
				Other ->
					{error, Other}
			end;
		{aborted, {already_exists,schema, _, disc_copies}} ->
			ok;
		Other ->
			{error, Other}
	end.