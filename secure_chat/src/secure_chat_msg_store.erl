-module(secure_chat_msg_store).
-export([start_link/0,
		store_offline_msg/1,
		get_offline_msgs/1,
		init/1,
		handle_cast/2,
		handle_call/3,
		handle_info/2,
		terminate/2,
		code_change/3]).
-include("secure_chat.hrl").
-behavior(gen_server).

%% === Messages ===

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store_offline_msg(Msg) ->
	gen_server:call(whereis(?MODULE), {store_offline_msg, Msg}).

get_offline_msgs(For) ->
	gen_server:call(whereis(?MODULE), {get_offline_msgs, For}).

%% === gen_server ===

init([]) ->
	{ok, {}}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_call({store_offline_msg, Msg}, _From, State) when is_record(Msg, message) ->
	F = fun() ->
		mnesia:write(Msg)
	end,
	mnesia:activity(transaction, F),
	{reply, ok, State};
handle_call({get_offline_msgs, For}, _From, State) ->
	F = fun() ->
		mnesia:read({message, For})
	end,
	{reply, mnesia:activity(transaction, F), State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_, _) ->
	ok.

code_change(_, State, _) ->
	{ok, State}.