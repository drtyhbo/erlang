-module(secure_chat_handler).
-export([start/2,
		receive_msg/3,
		init/1,
		terminate/3,
		logged_out/2,
		logged_in/2,
		handle_info/3,
		handle_json/3]).
-behavior(gen_fsm).

-record(handler_state,
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
	io:format("Connecting~n"),
	{ok, logged_out, #handler_state{socket = Socket,
			redis_connection = RedisConnection}}.

%% ==== Logged Out Events ====

logged_out(connect, State) ->
	secure_chat_msg_router:add_user(State#handler_state.username, self()),
	send_json(State#handler_state.socket, ?CONNECTED_JSON()),
	{next_state, logged_in, State};
logged_out(Event, _State) ->
	io:format("Received unknown logged_out event ~p~n", [Event]).

%% ==== Logged In Events ====

logged_in({receive_msg, From, Msg}, State) ->
	send_json(State#handler_state.socket, ?RECIEVE_MSG_JSON(From, Msg)),
	{next_state, logged_in, State};
logged_in(Event, _State) ->
	io:format("Received unknown logged_in event ~p~n", [Event]).

%% ==== Generic Events ====

handle_info(?SOCK(Msg), FSMState, State) ->
	Json = mochijson2:decode(Msg),
	NewState = handle_json(FSMState, Json, State),
	{next_state, FSMState, NewState};
handle_info(?SOCK_CLOSED(), _FSMState, State) ->
	{stop, disconnect, State};
handle_info(Info, FSMState, State) ->
	{next_state, FSMState, State}.

%% ==== JSON Handlers ====

handle_json(logged_out, {struct, ?CONNECT_JSON(Username, SessionToken)}, State) ->
	case eredis:q(State#handler_state.redis_connection, ["HGET", Username, "s"]) of
	 	{ok, RedisSessionToken} when RedisSessionToken =:= SessionToken ->
			connect(),
			State#handler_state{username = Username};
	 	_ ->
	 		State
	end;
handle_json(logged_in, {struct, ?SEND_MSG_JSON(To, Msg)}, State) ->
	secure_chat_msg_router:send_msg(State#handler_state.username, To, Msg),
	State;
handle_json(FSMState, Json, State) ->
	{next_state, FSMState, State}.

send_json(Socket, Json) ->
	gen_tcp:send(Socket, mochijson2:encode(Json)).

%% ==== Terminate ====

terminate(Reason, _StateName, State) ->
	secure_chat_msg_router:remove_user(State#handler_state.username),
	gen_tcp:close(State#handler_state.socket),
	io:format("~p~n", [Reason]),
	ok.