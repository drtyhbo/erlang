-module(secure_chat_serv).
-export([start_link/1,
		init/1,
		handle_cast/2,
		handle_call/3,
		handle_info/2,
		terminate/2,
		code_change/3]).
-behavior(gen_server).

-record(server,
		{listen_socket,
		redis_connection,
		accept_pid}).

start_link(Port) ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []) of
		{ok, Pid} ->
			io:format("Casting ~p~n", [Pid]),
			gen_server:cast(Pid, accept),
			{ok, Pid};
		Error ->
			Error
	end.

init([Port]) ->
	{ok, RedisConnection} = eredis:start_link(),
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}]),
	{ok, #server{listen_socket=Socket, redis_connection=RedisConnection}}.

handle_cast(accept, State) ->
	case gen_tcp:accept(State#server.listen_socket) of
		{ok, Socket} ->
			{ok, NewPid} = secure_chat_user:start(Socket, State#server.redis_connection),
			gen_tcp:controlling_process(Socket, NewPid),
			gen_server:cast(self(), accept),
			{noreply, State};
		_ ->
			{noreply, State}
	end.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_, State) ->
	lager:info("Terminating chat server."),
	gen_tcp:close(State#server.listen_socket).

code_change(_, State, _) ->
	{ok, State}.