-module(secure_chat_redis).
-export([user_id_to_key/1]).

user_id_to_key(UserId) ->
	"u:{" ++ integer_to_list(UserId) ++ "}".