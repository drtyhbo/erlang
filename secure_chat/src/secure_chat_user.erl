-module(secure_chat_user).
-export([start/2,
		receive_msg/2,
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
-define(IN_CONNECT_JSON(Username, SessionToken), {struct, [{<<"c">>, Username},{<<"s">>, SessionToken}]}).
-define(IN_SEND_MSG_JSON(To, Msg), {struct, [{<<"r">>, To},{<<"m">>, Msg}]}).

% Outgoing JSON
-define(OUT_CONNECTED_JSON(), {struct, [{<<"r">>, <<"connected">>}]}).
-define(OUT_SEND_MSG_JSON(TS, From, Msg), {struct, [{<<"d">>, TS},{<<"f">>, From},{<<"m">>, Msg}]}).
-define(OUT_ERROR_JSON(Error), {struct, [{<<"e">>, Error}]}).

%% === Messages ===

start(Socket, RedisConnection) ->
	gen_fsm:start(?MODULE, [Socket, RedisConnection], []).

connect() ->
	gen_fsm:send_event(self(), connect).

receive_msg(Pid, Msg) ->
	gen_fsm:send_event(Pid, {receive_msg, Msg}).

%% === gen_fsm ===

init([Socket, RedisConnection]) ->
	{ok, logged_out, #user_state{socket = Socket,
			redis_connection = RedisConnection}}.

handle_info(?RECEIVE_SOCK_MSG(Msg), FSMState, State) ->
	Json = mochijson2:decode(Msg),
	NewState = handle_json(FSMState, Json, State),
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
logged_out(Event, _State) ->
	io:format("Received unknown logged_out event ~p~n", [Event]).

logged_in(check_offline_msgs, State) ->
	send_msgs(State#user_state.socket, secure_chat_msg_store:get_offline_msgs(State#user_state.username)),
	{next_state, logged_in, State};
logged_in({receive_msg, Msg}, State) ->
	send_msgs(State#user_state.socket, [Msg]),
	{next_state, logged_in, State};
logged_in(Event, _State) ->
	io:format("Received unknown logged_in event ~p~n", [Event]).

%% ==== JSON Handlers ====

handle_json(logged_out, ?IN_CONNECT_JSON(Username, SessionToken), State) ->
	case eredis:q(State#user_state.redis_connection, ["HGET", Username, "s"]) of
	 	{ok, RedisSessionToken} when RedisSessionToken =:= SessionToken ->
			connect(),
			State#user_state{username = Username};
	 	_ ->
	 		State
	end;
handle_json(logged_in, ?IN_SEND_MSG_JSON(To, Msg), State) ->
	NewMsg = #message{ts=secure_chat_utils:timestamp(), from=State#user_state.username, to=To, msg=Msg},
	case route_msg(To, NewMsg) of
		ok ->
			State;
		_ ->
			send_json(State#user_state.socket, ?OUT_ERROR_JSON("send_message")),
			State
	end;
handle_json(FSMState, _, State) ->
	{next_state, FSMState, State}.

%% === Message routing ===

route_msg(To, Msg) ->
	case secure_chat_msg_router:send_msg(To, Msg) of
		ok ->
			ok;
		offline ->
			secure_chat_msg_store:store_offline_msg(Msg)
	end.

%% === Socket functions ===

send_msgs(_Socket, []) ->
	ok;
send_msgs(Socket, [H|T]) ->
	send_json(Socket, ?OUT_SEND_MSG_JSON(H#message.ts, H#message.from, H#message.msg)),
	send_msgs(Socket, T).

send_json(Socket, Json) ->
	gen_tcp:send(Socket, mochijson2:encode(Json)).