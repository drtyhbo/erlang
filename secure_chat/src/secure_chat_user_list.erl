-module(secure_chat_user_list).
-export([start_link/0,
		get_user_list/0,
		add_user/2,
		remove_user/1,
		init/1,
		handle_cast/2,
		handle_call/3,
		handle_info/2,
		terminate/2,
		code_change/3]).
-behavior(gen_server).

-record(user_list_state, {user_list}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_user_list() ->
	gen_server:call(whereis(?MODULE), user_list).

add_user(Username, Pid) ->
	gen_server:call(whereis(?MODULE), {add_user, Username, Pid}).

remove_user(Username) ->
	gen_server:call(whereis(?MODULE), {remove_user, Username}).

init([]) ->
	{ok, #user_list_state{user_list=ets:new(user_lookup, [])}}.

handle_cast(accept, State) ->
	{noreply, State}.

handle_call(user_list, _From, State) ->
	io:format("User list~n"),
	{reply, State#user_list_state.user_list, State};
handle_call({add_user, Username, Pid}, _From, State) ->
	io:format("Add user ~p ~p~n", [Username, Pid]),
	ets:insert(State#user_list_state.user_list, {Username, Pid}),
	{reply, ok, State};
handle_call({remove_user, Username}, _From, State) ->
	io:format("Remove user ~p~n", [Username]),
	ets:delete(State#user_list_state.user_list, Username),
	{reply, ok, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_, _) ->
	ok.

code_change(_, State, _) ->
	{ok, State}.