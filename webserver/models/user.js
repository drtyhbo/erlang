var redis = require('./redis').redis,
	utils = require('../utils/utils.js'),
	Promise = require('bluebird').Promise;

var userKeys = {
	phone: 'phone',
	code: 'code',
	session: 'session',
	active: 'active'
};

function userKeyFromId(id) {
	return 'u:{' + id + '}';
}

function phoneKeyFromPhoneNumber(phoneNumber) {
	return 'p:{' + phoneNumber + '}';
}

function userIdFromPhoneNumber(phoneNumber) {
	return redis.getAsync(phoneKeyFromPhoneNumber(phoneNumber));
}

function doesUserWithIdExist(userId) {
	return redis.existsAsync(userKeyFromId(userId)).then(function(response) {
		return response != 0 ? Promise.resolve() : Promise.reject('id');
	});
}

function friendRequestsKeyFromId(id) {
	return 'fr:{' + id + '}';
}

function friendListKeyFromId(id) {
	return 'f:{' + id + '}';
}

function isFriend(userId, friendUserId) {
	return redis.hgetAsync(friendListKeyFromId(userId), friendUserId).then(function(response) {
		return Promise.resolve(response != null);
	});
}

function addFriend(userId, friendUserId) {
	return redis.hsetAsync(friendListKeyFromId(userId), friendUserId, true).then(function() {
		return redis.hdelAsync(friendRequestsKeyFromId(userId), friendUserId);
	}).then(function() {
		return Promise.resolve(true);
	});
}

function addFriendAndRequest(userId, friendUserId) {
	return redis.multi()
			.hset(friendListKeyFromId(userId), friendUserId, true)
			.hdel(friendRequestsKeyFromId(userId), friendUserId)
			.execAsync().then(function() {
		return redis.hset(friendRequestsKeyFromId(friendUserId), userId, true);
	}).then(function(response) {
		if (response) {
			// Send notification
		}
		return Promise.resolve(true);
	});
}

function friendsForUserWithId(userId) {
	/*var friends = [];
	return redis.hgetallAsync(friendListKeyFromId(userId)).then(function(response) {
		var multi = redis.multi();
		for (id in response) {
			friends.push({
				id: id
			});
			multi.hget(userKeyFromId(id), 'name');
		}
		return multi.execAsync();
	}).then(function(names) {
		for (var i = names.length - 1; i >= 0; i--) {
			if (!names[i]) {
				friends.splice(i, 1);
			} else {
				friends[i].name = names[i]
			}
		}
		return Promise.resolve(friends);
	});*/
	return Promise.resolve([]);
}

function createNewUser(phoneNumber) {
	var code = Math.floor(Math.random() * 900000) + 100000;
	return redis.incrAsync('user_id').then(function(id) {
		return redis.setAsync(phoneKeyFromPhoneNumber(phoneNumber), id).thenReturn(id);
	}).then(function(id) {
		return redis.hmsetAsync(userKeyFromId(id), [userKeys.phone, phoneNumber]).thenReturn(id);
	});
}

exports.create = function(phoneNumber, cb) {
	var userId;
	userIdFromPhoneNumber(phoneNumber).then(function(id) {
		if (!id) {
			return createNewUser(phoneNumber);
		} else {
			return Promise.resolve(id);
		}
	}).then(function(id) {
		userId = id;
		return redis.hgetAsync(userKeyFromId(id), userKeys.code);
	}).then(function(code) {
		if (!code) {
			code = Math.floor(Math.random() * 900000) + 100000;
			return redis.hsetAsync(userKeyFromId(userId), userKeys.code, code).thenReturn(code);
		} else {
			return Promise.resolve(code);
		}
	}).then(function(code) {
		cb(null, code);
	}, function(err) {
		cb(err, null);
	});
};

exports.login = function(phoneNumber, code, cb) {
	var sharedId;
	userIdFromPhoneNumber(phoneNumber).then(function(id) {
		if (!id) {
			cb('unknown')
		} else {
			sharedId = id;
			return redis.hgetAsync(userKeyFromId(id), userKeys.code);
		}
	}).then(function(retrievedCode) {
		if (!code || retrievedCode != code) {
			return Promise.reject('mismatch');
		} else {
			return redis.hgetAsync(userKeyFromId(sharedId), userKeys.session);
		}
	}).then(function(sessionToken) {
		if (sessionToken) {
			return Promise.resolve(sessionToken);
		} else {
			sessionToken = utils.generateSessionToken();
			return redis.hmsetAsync(userKeyFromId(sharedId), userKeys.session, sessionToken, userKeys.active, true).thenReturn(sessionToken);
		}
	}).then(function(sessionToken) {
		cb(null, sharedId, sessionToken);
	}, function(err) {
		cb(err);
	});
};

exports.confirmSession = function(id, sessionToken, cb) {
	redis.hgetAsync(userKeyFromId(id), userKeys.session).then(function(dbSessionToken) {
		cb(null, sessionToken && dbSessionToken == sessionToken);
	}, function(err) {
		cb(err);
	});
};

function addFriendHelper(userId, friendUserId, canRequest) {
	if (!friendUserId) {
		return Promise.reject('phone');
	}

	return isFriend(friendUserId, userId).then(function(isFriend) {
		// If the other user already has me as a friend, we don't need to send a new
		// request.
		if (isFriend) {
			return addFriend(userId, friendUserId);
		} else if (canRequest) {
			return addFriendAndRequest(userId, friendUserId);
		} else {
			return Promise.reject('not_requested');
		}
	}).then(function(success) {
		return success ? Promise.resolve() : Promise.reject('error');
	});
}

exports.addFriendFromPhoneNumber = function(userId, friendPhoneNumber, cb) {
	userIdFromPhoneNumber(friendPhoneNumber).then(function(friendUserId) {
		return addFriendHelper(userId, friendUserId, true);
	}).then(function() {
		cb(null);
	}, function(err) {
		cb(err);
	});
};

exports.addFriendFromId = function(userId, friendUserId, cb) {
	doesUserWithIdExist(friendUserId).then(function() {
		return addFriendHelper(userId, friendUserId, false);
	}).then(function() {
		cb(null);
	}, function(err) {
		cb(err);
	});
};

exports.checkUsersWithPhoneNumbers = function(phoneNumbers, cb) {
	var promises = [];
	for (var i = 0, phoneNumber; phoneNumber = phoneNumbers[i]; i++) {
		promises.push(redis.existsAsync(phoneKeyFromPhoneNumber(phoneNumber)));
	}
	Promise.all(promises).then(function(exists) {
		console.log(exists);
		//cb(null, exists.map(function(doesExist) { return doesExist != 0; }));
	}, function(err) {
		cb(err);
	});
}

exports.getFriendsForUserWithId = function(id, cb) {
	friendsForUserWithId(id).then(function(friends) {
		cb(null, friends);
	}, function(err) {
		cb(err)
	});
};