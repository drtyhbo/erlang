var client = require('./redis').client,
	utils = require('../utils/utils.js'),
	Promise = require('bluebird').Promise;

function userKeyFromId(id) {
	return 'u' + id;
}

function phoneKeyFromPhoneNumber(phoneNumber) {
	return 'p' + phoneNumber;
}

function userIdFromPhoneNumber(phoneNumber) {
	return client.getAsync(phoneKeyFromPhoneNumber(phoneNumber));
}

function doesUserWithIdExist(userId) {
	return client.existsAsync(userKeyFromId(userId)).then(function(response) {
		return response != 0 ? Promise.resolve() : Promise.reject('id');
	});
}

function friendRequestsKeyFromId(id) {
	return 'fR' + id;
}

function friendListKeyFromId(id) {
	return 'f' + id;
}

function isFriend(userId, friendUserId) {
	return client.hgetAsync(friendListKeyFromId(userId), friendUserId).then(function(response) {
		return Promise.resolve(response != null);
	});
}

function addFriend(userId, friendUserId) {
	return client.multi()
			.hset(friendListKeyFromId(userId), friendUserId, true)
			.hdel(friendRequestsKeyFromId(userId), friendUserId)
			.execAsync()
			.then(function() {
				return Promise.resolve(true);
			});
}

function addFriendAndRequest(userId, friendUserId) {
	return client.multi()
			.hset(friendListKeyFromId(userId), friendUserId, true)
			.hdel(friendRequestsKeyFromId(userId), friendUserId)
			.hset(friendRequestsKeyFromId(friendUserId), userId, true)
			.execAsync()
			.then(function(response) {
				if (response[2]) {
					// Send notification.
				}
				return Promise.resolve(true);
			});
}

function createNewUser(phoneNumber) {
	var code = Math.floor(Math.random() * 900000) + 100000;
	return client.incrAsync('user_id').then(function(id) {
		return client.setAsync(phoneKeyFromPhoneNumber(phoneNumber), id).thenReturn(id);
	}).then(function(id) {
		return client.hmsetAsync(userKeyFromId(id), ['phoneNumber', phoneNumber]).thenReturn(id);
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
		return client.hgetAsync(userKeyFromId(id), 'code');
	}).then(function(code) {
		if (!code) {
			code = Math.floor(Math.random() * 900000) + 100000;
			return client.hsetAsync(userKeyFromId(userId), 'code', code).thenReturn(code);
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
			return client.hgetAsync(userKeyFromId(id), 'code');
		}
	}).then(function(retrievedCode) {
		if (!code || retrievedCode != code) {
			return Promise.reject('mismatch');
		} else {
			return client.hgetAsync(userKeyFromId(sharedId), 'session');
		}
	}).then(function(sessionToken) {
		if (sessionToken) {
			return Promise.resolve(sessionToken);
		} else {
			sessionToken = utils.generateSessionToken();
			return client.hmsetAsync(userKeyFromId(sharedId), 'session', sessionToken, 'active', true).thenReturn(sessionToken);
		}
	}).then(function(sessionToken) {
		cb(null, sharedId, sessionToken);
	}, function(err) {
		cb(err);
	});
};

exports.confirmSession = function(id, sessionToken, cb) {
	client.hgetAsync(userKeyFromId(id), 'session').then(function(dbSessionToken) {
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
	var multi = client.multi();
	for (var i = 0, phoneNumber; phoneNumber = phoneNumbers[i]; i++) {
		multi.exists(phoneKeyFromPhoneNumber(phoneNumber));
	}
	multi.execAsync().then(function(exists) {
		cb(null, exists.map(function(doesExist) { return doesExist != 0; }));
	}, function(err) {
		cb(err);
	});
}