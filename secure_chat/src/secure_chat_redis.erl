-module(secure_chat_redis).
-export([device_id_to_key/1,
		user_id_to_key/1]).

device_id_to_key(DeviceId) ->
	"d:{" ++ integer_to_list(DeviceId) ++ "}".

user_id_to_key(UserId) ->
	"u:{" ++ integer_to_list(UserId) ++ "}".