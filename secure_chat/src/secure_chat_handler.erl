-module(secure_chat_handler).
-export([start/2, init/1, terminate/3, logged_out/2, logged_in/2, handle_info/3, handle_json/3]).
-behavior(gen_fsm).

-record(handler_state,
		{socket,
		redis_connection,
		username,
		user_list}).

%% ==== Defines ====

-define(SOCK(Msg), {tcp, _Port, Msg}).
-define(SOCK_CLOSED(Msg), {tcp_closed, _Port, Msg}).

% Incoming JSON
-define(CONNECT_JSON(Username, SessionToken), [{<<"c">>, Username},{<<"s">>, SessionToken}]).
-define(SEND_MESSAGE_JSON(Recipient, Message), [{<<"r">>, Recipient},{<<"m">>, Message}]).

% Outgoing JSON
-define(CONNECTED_JSON(), {struct, [{<<"r">>, <<"connected">>}]}).
-define(RECIEVE_MESSAGE_JSON(Sender, Message), {struct, [{<<"s">>, Sender},{<<"m">>, Message}]}).

start(Socket, RedisConnection) ->
	gen_fsm:start(?MODULE, [Socket, RedisConnection], []).

%% === Messages ===

connect() ->
	gen_fsm:send_event(self(), connect).

send_message(Recipient, Message, State) ->
	case ets:lookup(State#handler_state.user_list, Recipient) of
		[{_, Pid}] ->
			gen_fsm:send_event(Pid, {receive_message, State#handler_state.username, Message});
		[] ->
			not_found
	end.

%% === gen_fsm ===

init([Socket, RedisConnection]) ->
	io:format("Connecting~n"),
	UserList = secure_chat_user_list:get_user_list(),
	{ok, logged_out, #handler_state{socket = Socket,
			redis_connection = RedisConnection,
			user_list = UserList}}.

%% ==== Logged Out Events ====

logged_out(connect, State) ->
	secure_chat_user_list:add_user(State#handler_state.username, self()),
	send_json(State#handler_state.socket, ?CONNECTED_JSON()),
	{next_state, logged_in, State};
logged_out(Event, _State) ->
	io:format("Received unknown logged_out event ~p~n", [Event]).

%% ==== Logged In Events ====

logged_in({receive_message, Sender, Message}, State) ->
	io:format("Received message ~p~n", [Message]),
	send_json(State#handler_state.socket, ?RECIEVE_MESSAGE_JSON(Sender, Message)),
	{next_state, logged_in, State};
logged_in(Event, _State) ->
	io:format("Received unknown logged_in event ~p~n", [Event]).

%% ==== Generic Events ====

handle_info(?SOCK(Msg), FSMState, State) ->
	io:format("handle_info~n"),
	Json = mochijson2:decode(Msg),
	NewState = handle_json(FSMState, Json, State),
	{next_state, FSMState, NewState};
handle_info(?SOCK_CLOSED(_Msg), _FSMState, State) ->
	io:format("Socket disconnected ~n"),
	{stop, disconnect, State};
handle_info(Info, FSMState, State) ->
	io:format("Received unknown handle_info ~p,~p,~p~n", [Info, FSMState, State]),
	{next_state, FSMState, State}.

%% ==== JSON Handlers ====

handle_json(logged_out, {struct, ?CONNECT_JSON(Username, SessionToken)}, State) ->
	io:format("handle_json logged_out ~p ~p~n", [Username, SessionToken]),
	 case eredis:q(State#handler_state.redis_connection, ["HGET", Username, "s"]) of
	 	{ok, RedisSessionToken} when RedisSessionToken =:= SessionToken ->
			connect(),
			State#handler_state{username = Username};
	 	_ ->
	 		State
	end;
handle_json(logged_in, {struct, ?SEND_MESSAGE_JSON(Recipient, Message)}, State) ->
	io:format("Send message ~p ~p~n", [Recipient, Message]),
	send_message(Recipient, Message, State),
	State;
handle_json(FSMState, Json, State) ->
	io:format("Unknown handle_json ~p ~p~n", [FSMState, Json]),
	{next_state, FSMState, State}.

send_json(Socket, Json) ->
	gen_tcp:send(Socket, mochijson2:encode(Json)).

%% ==== Terminate ====

terminate(Reason, _StateName, State) ->
	secure_chat_serv:remove_user(State#handler_state.username),
	gen_tcp:close(State#handler_state.socket),
	io:format("~p~n", [Reason]),
	ok.