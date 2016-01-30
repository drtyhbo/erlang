-module(secure_chat_msg_router).
-export([start_link/0,
		add_user/2,
		remove_user/1,
		route_msg/1,
		msg_is_received/1,
		init/1,
		handle_cast/2,
		handle_call/3,
		handle_info/2,
		terminate/2,
		code_change/3]).
-include("secure_chat.hrl").
-behavior(gen_server).

-define(OFFLINE_INTERVAL, 5000).

-record(msg_router_state, {user_list, pending_msgs, curr_msg_id}).
-record(routing_info, {local_id}).

%% === Messages ===

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_user(Username, Pid) ->
	gen_server:call(whereis(?MODULE), {add_user, Username, Pid}).

remove_user(Username) ->
	gen_server:call(whereis(?MODULE), {remove_user, Username}).

route_msg(Msg) ->
	gen_server:call(whereis(?MODULE), {route_msg, Msg}).

msg_is_received(Msg) ->
	gen_server:call(whereis(?MODULE), {msg_is_received, Msg}).

%% === gen_server ===

init([]) ->
	{ok, #msg_router_state{
		user_list=ets:new(user_lookup, []),
		pending_msgs=ets:new(pending_msgs, [{keypos, 3}]),
		curr_msg_id=0}}.

handle_cast(accept, State) ->
	{noreply, State}.

handle_call({add_user, Username, Pid}, _From, State) ->
	ets:insert(State#msg_router_state.user_list, {Username, Pid}),
	{reply, ok, State};
handle_call({remove_user, Username}, _From, State) ->
	ets:delete(State#msg_router_state.user_list, Username),
	{reply, ok, State};
handle_call({route_msg, Msg}, _From, State) ->
	CurrMsgId = State#msg_router_state.curr_msg_id,
	NewMsg = Msg#message{routing_info=#routing_info{local_id=CurrMsgId}},
	case ets:lookup(State#msg_router_state.user_list, Msg#message.to) of
	[{_, Pid}] ->
		ets:insert(State#msg_router_state.pending_msgs, NewMsg),
		secure_chat_user:receive_msg(Pid, NewMsg),
		erlang:send_after(?OFFLINE_INTERVAL, whereis(?MODULE), {check_offline_msg, NewMsg}),
		{reply, ok, State#msg_router_state{curr_msg_id=CurrMsgId + 1}};
	_ ->
		store_offline_msg(NewMsg),
		{reply, offline, State#msg_router_state{curr_msg_id=CurrMsgId + 1}}
	end;
handle_call({msg_is_received, Msg}, _From, State) ->
	ets:delete(State#msg_router_state.pending_msgs, Msg#message.routing_info),
	secure_chat_user:msg_is_delivered(Msg),
	{reply, ok, State}.

handle_info({check_offline_msg, Msg}, State) ->
	RoutingInfo = Msg#message.routing_info,
	case ets:lookup(State#msg_router_state.pending_msgs, RoutingInfo) of
		[] ->
			ok;
		_ ->
			store_offline_msg(Msg),
			ets:delete(State#msg_router_state.pending_msgs, RoutingInfo)
	end,
	{noreply, State};
handle_info(Info, State) ->
	io:format("Recieved unknown message ~p~n", Info),
	{noreply, State}.

terminate(_, _) ->
	ok.

code_change(_, State, _) ->
	{ok, State}.

%% ==== Store offline messages ===

store_offline_msg(Msg) ->
	secure_chat_msg_store:store_offline_msg(Msg),
	secure_chat_user:msg_is_delivered(Msg).