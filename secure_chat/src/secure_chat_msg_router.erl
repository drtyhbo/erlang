-module(secure_chat_msg_router).
-export([start_link/0,
		add_user/2,
		remove_user/1,
		send_msg/3,
		init/1,
		handle_cast/2,
		handle_call/3,
		handle_info/2,
		terminate/2,
		code_change/3]).
-behavior(gen_server).

-record(msg_router_state, {user_list}).

%% === Messages ===

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_user(Username, Pid) ->
	gen_server:call(whereis(?MODULE), {add_user, Username, Pid}).

remove_user(Username) ->
	gen_server:call(whereis(?MODULE), {remove_user, Username}).

send_msg(From, To, Msg) ->
	gen_server:call(whereis(?MODULE), {send_msg, From, To, Msg}).

%% === gen_server ===

init([]) ->
	{ok, #msg_router_state{user_list=ets:new(user_lookup, [])}}.

handle_cast(accept, State) ->
	{noreply, State}.

handle_call({add_user, Username, Pid}, _From, State) ->
	ets:insert(State#msg_router_state.user_list, {Username, Pid}),
	{reply, ok, State};
handle_call({remove_user, Username}, _From, State) ->
	ets:delete(State#msg_router_state.user_list, Username),
	{reply, ok, State};
handle_call({send_msg, From, To, Msg}, _From, State) ->
	case ets:lookup(State#msg_router_state.user_list, To) of
	[{_, Pid}] ->
		secure_chat_user:receive_msg(Pid, From, Msg),
		{reply, ok, State};
	_ ->
		{reply, ok, State}
	end.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_, _) ->
	ok.

code_change(_, State, _) ->
	{ok, State}.