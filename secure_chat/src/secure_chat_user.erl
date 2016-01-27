-module(secure_chat_user).
-export([start/2,
		receive_msg/3,
		init/1,
		handle_info/3,
		handle_event/3,
		handle_sync_event/4,
		code_change/4,
		terminate/3,
		logged_out/2,
		logged_in/2,
		handle_json/3]).
-behavior(gen_fsm).

-record(user_state,
		{socket,
		redis_connection,
		username}).

%% ==== Defines ====

-define(SOCK(Msg), {tcp, _Port, Msg}).
-define(SOCK_CLOSED(), {tcp_closed, _Port}).

% Incoming JSON
-define(CONNECT_JSON(Username, SessionToken), [{<<"c">>, Username},{<<"s">>, SessionToken}]).
-define(SEND_MSG_JSON(To, Msg), [{<<"r">>, To},{<<"m">>, Msg}]).

% Outgoing JSON
-define(CONNECTED_JSON(), {struct, [{<<"r">>, <<"connected">>}]}).
-define(RECIEVE_MSG_JSON(From, Msg), {struct, [{<<"f">>, From},{<<"m">>, Msg}]}).

%% === Messages ===

start(Socket, RedisConnection) ->
	gen_fsm:start(?MODULE, [Socket, RedisConnection], []).

connect() ->
	gen_fsm:send_event(self(), connect).

receive_msg(Pid, From, Msg) ->
	gen_fsm:send_event(Pid, {receive_msg, From, Msg}).

%% === gen_fsm ===

init([Socket, RedisConnection]) ->
	{ok, logged_out, #user_state{socket = Socket,
			redis_connection = RedisConnection}}.

handle_info(?SOCK(Msg), FSMState, State) ->
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

terminate(Reason, _StateName, State) ->
	secure_chat_msg_router:remove_user(State#user_state.username),
	gen_tcp:close(State#user_state.socket),
	ok.

%% ==== FSM Events ====

logged_out(connect, State) ->
	secure_chat_msg_router:add_user(State#user_state.username, self()),
	send_json(State#user_state.socket, ?CONNECTED_JSON()),
	{next_state, logged_in, State};
logged_out(Event, _State) ->
	io:format("Received unknown logged_out event ~p~n", [Event]).

logged_in({receive_msg, From, Msg}, State) ->
	send_json(State#user_state.socket, ?RECIEVE_MSG_JSON(From, Msg)),
	{next_state, logged_in, State};
logged_in(Event, _State) ->
	io:format("Received unknown logged_in event ~p~n", [Event]).

%% ==== JSON Handlers ====

handle_json(logged_out, {struct, ?CONNECT_JSON(Username, SessionToken)}, State) ->
	case eredis:q(State#user_state.redis_connection, ["HGET", Username, "s"]) of
	 	{ok, RedisSessionToken} when RedisSessionToken =:= SessionToken ->
			connect(),
			State#user_state{username = Username};
	 	_ ->
	 		State
	end;
handle_json(logged_in, {struct, ?SEND_MSG_JSON(To, Msg)}, State) ->
	secure_chat_msg_router:send_msg(State#user_state.username, To, Msg),
	State;
handle_json(FSMState, _, State) ->
	{next_state, FSMState, State}.

send_json(Socket, Json) ->
	gen_tcp:send(Socket, mochijson2:encode(Json)).