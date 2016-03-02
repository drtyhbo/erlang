-module(secure_chat_pns).
-export([start_link/0,
		send_notification/2,
		init/1,
		handle_cast/2,
		handle_call/3,
		handle_info/2,
		terminate/2,
		code_change/3,
		handle_apns_error/2,
		handle_apns_delete_subscription/1]).
-behavior(gen_server).
-include_lib("apns/include/apns.hrl").


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


send_notification(UserId, Message) ->
	gen_server:cast(whereis(?MODULE), {send_notification, UserId, Message}).


init([]) ->
	case apns:connect(ios_apns,
        fun ?MODULE:handle_apns_error/2,
        fun ?MODULE:handle_apns_delete_subscription/1) of
		{ok, _Pid} ->
			{ok, {}};
		{error, {already_started, _Pid}} ->
			{ok, {}};
		{error, Reason} ->
			{error, Reason}
	end.


handle_cast({send_notification, UserId, Message}, State) ->
	case eredis_cluster:q(["HGET", secure_chat_redis:user_id_to_key(UserId), "iosToken"]) of
	 	{ok, DeviceToken} ->
	 		apns:send_message(ios_apns, #apns_msg{
	 			alert = Message,
	 			content_available = true,
				device_token = binary_to_list(DeviceToken)});
	 	_ ->
	 		ok
	end,
	{noreply, State};
handle_cast(_Request, State) ->
	{noreply, State}.


handle_call(_Request, _From, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.


terminate(_, _State) ->
	ok.


code_change(_, State, _) ->
	{ok, State}.


handle_apns_error(MsgId, Status) ->
	io:format("apns error: ~p - ~p~n", [MsgId, Status]).


handle_apns_delete_subscription(Data) ->
	io:format("apns delete subscription: ~p~n", [Data]).