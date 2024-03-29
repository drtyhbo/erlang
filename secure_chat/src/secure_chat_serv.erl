-module(secure_chat_serv).
-export([start_link/1,
		init/1,
		handle_cast/2,
		handle_call/3,
		handle_info/2,
		terminate/2,
		code_change/3]).
-behavior(gen_server).

-record(server, {listen_socket}).

start_link(Port) ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []) of
		{ok, Pid} ->
			gen_server:cast(Pid, accept),
			{ok, Pid};
		Error ->
			Error
	end.

init([Port]) ->
	{ok, Socket} = ssl:listen(Port, [
		{certfile, "/etc/certs/ssl-cert.pem"},
		{keyfile, "/etc/certs/ssl-privkey.pem"},
		{mode, binary},
		{active, true},
		{reuseaddr, true}]),
	{ok, #server{listen_socket=Socket}}.

handle_cast(accept, State) ->
	case ssl:transport_accept(State#server.listen_socket) of
		{ok, Socket} ->
			{ok, NewPid} = secure_chat_user:start(Socket),
			ssl:controlling_process(Socket, NewPid),
			ssl:ssl_accept(Socket),
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
	ssl:close(State#server.listen_socket).

code_change(_, State, _) ->
	{ok, State}.
