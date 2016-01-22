-module(chat_serv).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behavior(gen_server).

-record(server,
		{listen_socket,
		accept_pid}).

start_link(Port) ->
	case gen_server:start_link(?MODULE, [Port], []) of
		{ok, Pid} ->
			start_accept(Pid);
		Error ->
			Error
	end.

start_accept(Pid) ->
	case gen_server:call(Pid, {accept, Pid}) of
		ok ->
			{ok, Pid};
		Error ->
			Error
	end.

init([Port]) ->
	case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]) of
		{ok, Socket} ->
			process_flag(trap_exit, true),
			{ok, #server{listen_socket=Socket}};
		{error, Reason} ->
			{stop, Reason}
	end.

handle_call({accept, ServerPid}, _From, #server{listen_socket = ListenSocket} = State) ->
	io:format("Accept ~p~n", [ServerPid]),
	NewAcceptPid = spawn(fun() -> accept(ListenSocket, ServerPid) end),
	{reply, ok, State#server{accept_pid = NewAcceptPid}};
handle_call({connect, ServerPid}, _From, #server{listen_socket = ListenSocket} = State) ->
	io:format("Connect ~p~n", [ServerPid]),
	NewAcceptPid = spawn(fun() -> accept(ListenSocket, ServerPid) end),
	{reply, ok, State#server{accept_pid = NewAcceptPid}}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, State) ->
	io:format("Terminate ~p~n", [Reason]),
	gen_tcp:close(State#server.listen_socket).

code_change(_, State, _) ->
	{ok, State}.

accept(ListenSocket, ServerPid) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
			gen_tcp:close(Socket),
			io:format("Closing socket ~p~n", [Socket]),
			gen_server:call(ServerPid, {connect, ServerPid});
		Error ->
			io:format("Error accepting connection ~p~n", [Error])
	end.