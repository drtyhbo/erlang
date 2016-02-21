-module(secure_chat_user).
-export([start/1,
		receive_msg/2,
		msg_is_delivered/1,
		init/1,
		handle_info/3,
		handle_event/3,
		handle_sync_event/4,
		code_change/4,
		terminate/3,
		logged_out/2,
		logged_in/2,
		handle_json/3]).
-include("secure_chat.hrl").
-behavior(gen_fsm).

-define(OFFLINE_INTERVAL, 5000).

-record(user_state,
		{socket,
		user_id,
		local_id}).
-record(routing_info, {local_id}).


%% ==== Defines ====


-define(RECEIVE_SOCK_MSG(Msg), {tcp, _Port, Msg}).
-define(SOCK_CLOSED(), {tcp_closed, _Port}).

% Incoming JSON
-define(IN_CONNECT_JSON(UserId, SessionToken), [{<<"s">>, SessionToken}, {<<"c">>, UserId}]).
-define(IN_MSG_JSON(ClientId, To, Msg), [{<<"i">>, ClientId}, {<<"r">>, To}, {<<"m">>, Msg}]).

% Outgoing JSON
-define(OUT_CONNECTED_JSON(), {struct, [{<<"r">>, <<"connected">>}]}).
-define(OUT_CONNECTION_FAILED_JSON(), {struct, [{<<"r">>, <<"not_connected">>}]}).
-define(OUT_MSG_JSON(TS, From, Msg), {struct, [{<<"d">>, TS},{<<"f">>, From},{<<"m">>, Msg}]}).
-define(OUT_MSGS_JSON(Msgs), {struct, [{<<"m">>, Msgs}]}).
-define(OUT_OFFLINE_MSGS_JSON(Msgs), {struct, [{<<"o">>, Msgs}]}).
-define(OUT_ERROR_JSON(Error), {struct, [{<<"e">>, Error}]}).
-define(OUT_MSG_IS_DELIVERED(MsgId), {struct, [{<<"did">>, MsgId}]}).


%% === Messages ===


start(Socket) ->
	gen_fsm:start(?MODULE, [Socket], []).

connect() ->
	gen_fsm:send_event(self(), connect).

% Called to receive a message from another user.
receive_msg(Pid, Msg) ->
	gen_fsm:send_event(Pid, {receive_msg, Msg}).

% Called to indicate the specified message has been delivered.
msg_is_delivered(Msg) ->
	gen_fsm:send_event(Msg#message.from_pid, {msg_is_delivered, Msg}).


%% === gen_fsm ===


init([Socket]) ->
	UserState = #user_state{
			socket = Socket,
			local_id = 0},
	{ok, logged_out, UserState}.


handle_info(?RECEIVE_SOCK_MSG(Msg), FSMState, State) ->
	FlatJson = secure_chat_utils:flatten(mochijson2:decode(Msg)),
	NewState = handle_json(FSMState, FlatJson, State),
	{next_state, FSMState, NewState};
handle_info(?SOCK_CLOSED(), _FSMState, State) ->
	{stop, disconnect, State};
handle_info(_, FSMState, State) ->
	{next_state, FSMState, State}.


handle_event(_, FSMState, State) ->
	{next_state, FSMState, State}.


handle_sync_event(_, _, FSMState, State) ->
	{next_state, FSMState, State}.


code_change(_, FSMState, State, _) ->
	{ok, FSMState, State}.


terminate(_Reason, _StateName, State) ->
	gen_tcp:close(State#user_state.socket),
	ok.


%% ==== FSM Events ====


logged_out(connect, State) ->
	add_user(State#user_state.user_id, self()),
	send_json(State#user_state.socket, ?OUT_CONNECTED_JSON()),
	gen_fsm:send_event(self(), check_offline_msgs),
	{next_state, logged_in, State};
logged_out(Event, State) ->
	io:format("Received unknown logged_out event ~p~n", [Event]),
	{next_state, logged_out, State}.


logged_in(check_offline_msgs, State) ->
	case secure_chat_msg_store:get_offline_msgs(State#user_state.user_id) of
		[] ->
			ok;
		Msgs ->
			receive_offline_msgs(State#user_state.socket, Msgs)
	end,
	{next_state, logged_in, State};
logged_in({receive_msg, Msg}, State) ->
	secure_chat_msg_store:delete_offline_msg(Msg),
	receive_msgs(State#user_state.socket, [Msg]),
	{next_state, logged_in, State};
logged_in({msg_is_delivered, Msg}, State) ->
	send_json(State#user_state.socket, ?OUT_MSG_IS_DELIVERED(Msg#message.client_id)),
	{next_state, logged_in, State};
logged_in(Event, State) ->
	io:format("Received unknown logged_in event ~p~n", [Event]),
	{next_state, logged_in, State}.


%% ==== JSON Handlers ====


handle_json(logged_out, Json, State) ->
	Type = proplists:get_value(<<"t">>, Json),
	case Type of
		<<"c">> ->
			handle_connect_json(Json, State);
		Unknown ->
			io:format("Unknown logged_out message type ~p ~p ~n", [Unknown, Json]),
			State
	end;
handle_json(logged_in, Json, State) ->
	Type = proplists:get_value(<<"t">>, Json),
	case Type of
		<<"m">> ->
			handle_msg_json(Json, State);
		<<"a">> ->
			handle_acknowledgement_json(Json, State);
		Unknown ->
			io:format("Unknown logged_in message type ~p ~p ~n", [Unknown, Json]),
			State
	end;
handle_json(FSMState, Json, State) ->
	io:format("Unknown JSON received ~p~n", [Json]),
	{next_state, FSMState, State}.


%% === Message routing ===


add_user(UserId, Pid) ->
	syn:register(UserId, Pid).


%% === JSON Parsing ===


handle_connect_json(Json, State) ->
	UserId = proplists:get_value(<<"u">>, Json),
	SessionToken = proplists:get_value(<<"s">>, Json),
	case eredis_cluster:q(["HMGET", secure_chat_redis:user_id_to_key(UserId), "session"]) of
	 	{ok, [RedisSessionToken]} when RedisSessionToken =:= SessionToken ->
			connect(),
			State#user_state{user_id = UserId};
	 	_ ->
	 		send_json(State#user_state.socket, ?OUT_CONNECTION_FAILED_JSON()),
	 		State
	 end.


handle_msg_json(Json, State) ->
	To = proplists:get_value(<<"r">>, Json),
	ClientId = proplists:get_value(<<"i">>, Json),
	Msg = proplists:get_value(<<"m">>, Json),
	LocalId = State#user_state.local_id,
	NewMsg = #message{
		to=To,
		routing_info=#routing_info{local_id=LocalId},
		from=State#user_state.user_id,
		from_pid=self(),
		ts=secure_chat_utils:timestamp(),
		client_id=ClientId,
		msg=Msg},
	store_offline_msg(NewMsg),
	msg_is_delivered(NewMsg),
	case syn:find_by_key(To) of
	Pid when is_pid(Pid) ->
		secure_chat_user:receive_msg(Pid, NewMsg);
	_ ->
		ok
	end,
	%%secure_chat_pns:send_notification(To, State#user_state.name ++ " has sent you a message"),
	State#user_state{local_id=LocalId + 1}.


handle_acknowledgement_json(Json, State) ->
	case proplists:get_value(<<"w">>, Json) of
		<<"offline">> ->
			secure_chat_msg_store:delete_offline_msgs(State#user_state.user_id);
		DeleteType ->
			io:format("Unknown delete message type ~p~n", [DeleteType])
	end,
	State.


%% === Offline messages ===


store_offline_msg(Msg) ->
	secure_chat_msg_store:store_offline_msg(Msg).


%% === Socket functions ===


receive_msgs(Socket, Msgs) ->
	JsonList = lists:reverse(generate_msgs_json(Msgs, [])),
	send_json(Socket, ?OUT_MSGS_JSON(JsonList)).


receive_offline_msgs(Socket, Msgs) ->
	JsonList = lists:reverse(generate_msgs_json(Msgs, [])),
	send_json(Socket, ?OUT_OFFLINE_MSGS_JSON(JsonList)).


generate_msgs_json([], JsonList) ->
	JsonList;
generate_msgs_json([H|T], JsonList) ->
	generate_msgs_json(T, [?OUT_MSG_JSON(H#message.ts, H#message.from, H#message.msg)|JsonList]).


send_json(Socket, Json) ->
	gen_tcp:send(Socket, mochijson2:encode(Json) ++ "\n").