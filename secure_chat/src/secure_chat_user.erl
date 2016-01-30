-module(secure_chat_user).
-export([start/2,
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

-record(user_state,
		{socket,
		redis_connection,
		username}).

%% ==== Defines ====

-define(RECEIVE_SOCK_MSG(Msg), {tcp, _Port, Msg}).
-define(SOCK_CLOSED(), {tcp_closed, _Port}).

% Incoming JSON
-define(IN_CONNECT_JSON(Username, SessionToken), [{<<"c">>, Username}, {<<"s">>, SessionToken}]).
-define(IN_MSG_JSON(ClientId, To, Msg), [{<<"i">>, ClientId}, {<<"r">>, To}, {<<"m">>, Msg}]).

% Outgoing JSON
-define(OUT_CONNECTED_JSON(), {struct, [{<<"r">>, <<"connected">>}]}).
-define(OUT_SEND_MSG_JSON(TS, From, Msg), {struct, [{<<"d">>, TS},{<<"f">>, From},{<<"m">>, {struct, Msg}}]}).
-define(OUT_ERROR_JSON(Error), {struct, [{<<"e">>, Error}]}).
-define(OUT_MSG_IS_DELIVERED(MsgId), {struct, [{<<"did">>, MsgId}]}).

%% === Messages ===

start(Socket, RedisConnection) ->
	gen_fsm:start(?MODULE, [Socket, RedisConnection], []).

connect() ->
	gen_fsm:send_event(self(), connect).

receive_msg(Pid, Msg) ->
	gen_fsm:send_event(Pid, {receive_msg, Msg}).

msg_is_delivered(Msg) ->
	gen_fsm:send_event(Msg#message.from_pid, {msg_is_delivered, Msg}).

%% === gen_fsm ===

init([Socket, RedisConnection]) ->
	{ok, logged_out, #user_state{socket = Socket,
			redis_connection = RedisConnection}}.

flatten({struct, L}) ->
    flatten(L);
flatten([H | T]) ->
    [flatten(H) | flatten(T)];
flatten({K, V}) ->
    {K, flatten(V)};
flatten(Term) ->
    Term.

handle_info(?RECEIVE_SOCK_MSG(Msg), FSMState, State) ->
	FlatJson = flatten(mochijson2:decode(Msg)),
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
	secure_chat_msg_router:remove_user(State#user_state.username),
	gen_tcp:close(State#user_state.socket),
	ok.

%% ==== FSM Events ====

logged_out(connect, State) ->
	secure_chat_msg_router:add_user(State#user_state.username, self()),
	send_json(State#user_state.socket, ?OUT_CONNECTED_JSON()),
	gen_fsm:send_event(self(), check_offline_msgs),
	{next_state, logged_in, State};
logged_out(Event, State) ->
	io:format("Received unknown logged_out event ~p~n", [Event]),
	{next_state, logged_out, State}.

logged_in(check_offline_msgs, State) ->
	send_msgs(State#user_state.socket, secure_chat_msg_store:get_offline_msgs(State#user_state.username)),
	{next_state, logged_in, State};
logged_in({receive_msg, Msg}, State) ->
	secure_chat_msg_router:msg_is_received(Msg),
	send_msgs(State#user_state.socket, [Msg]),
	{next_state, logged_in, State};
logged_in({msg_is_delivered, Msg}, State) ->
	send_json(State#user_state.socket, ?OUT_MSG_IS_DELIVERED(Msg#message.client_id)),
	{next_state, logged_in, State};
logged_in(Event, State) ->
	io:format("Received unknown logged_in event ~p~n", [Event]),
	{next_state, logged_in, State}.

%% ==== JSON Handlers ====

handle_json(logged_out, ?IN_CONNECT_JSON(Username, SessionToken), State) ->
	case eredis:q(State#user_state.redis_connection, ["HGET", Username, "s"]) of
	 	{ok, RedisSessionToken} when RedisSessionToken =:= SessionToken ->
			connect(),
			State#user_state{username = Username};
	 	_ ->
	 		State
	 end;
handle_json(logged_in, ?IN_MSG_JSON(ClientId, To, Msg), State) ->
	secure_chat_msg_router:route_msg(#message{
		to=To,
		from=State#user_state.username,
		from_pid=self(),
		ts=secure_chat_utils:timestamp(),
		client_id=ClientId,
		msg=Msg}),
	State;
handle_json(FSMState, Json, State) ->
	io:format("Unknown JSON received ~p~n", [Json]),
	{next_state, FSMState, State}.

%% === Socket functions ===

send_msgs(_Socket, []) ->
	ok;
send_msgs(Socket, [H|T]) ->
	send_json(Socket, ?OUT_SEND_MSG_JSON(H#message.ts, H#message.from, H#message.msg)),
	send_msgs(Socket, T).

send_json(Socket, Json) ->
	gen_tcp:send(Socket, mochijson2:encode(Json)).