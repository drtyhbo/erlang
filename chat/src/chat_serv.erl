-module(chat_serv).
-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).
-behaviour(gen_server).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init([]) ->
	{ok, []}.

handle_info(_Msg, State) ->
	{noreply, State}.

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
 
terminate(_Reason, _State) ->
	ok.