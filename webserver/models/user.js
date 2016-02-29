var redis = require('./redis').redis,
	utils = require('../utils/utils.js'),
	Promise = require('bluebird').Promise;

var userKeys = {
	phone: 'phone',
	code: 'code',
	session: 'session',
	active: 'active',
	key: 'key',
	iosPushToken: 'iosToken',
	firstName: 'firstName',
	lastName: 'lastName'
};

function userKeyFromId(id) {
	return 'u:{' + id + '}';
}

function phoneKeyFromPhoneNumber(phoneNumber) {
	return 'p:{' + phoneNumber + '}';
}

function preKeysKeyFromId(id) {
	return 'pk:{' + id + '}';
}

function preKeyIndicesKeyFromId(id) {
	return 'pki:{' + id + '}';
}

function userIdFromPhoneNumber(phoneNumber) {
	return redis.getAsync(phoneKeyFromPhoneNumber(phoneNumber));
}

function doesUserWithIdExist(userId) {
	return redis.existsAsync(userKeyFromId(userId)).then(function(response) {
		return response != 0 ? Promise.resolve() : Promise.reject('id');
	});
}

function getUserInfo(userId, info) {
	return redis.hmgetAsync(userKeyFromId(userId), info);
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

exports.login = function(phoneNumber, code, preKeys, cb) {
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
			return redis.hmgetAsync(userKeyFromId(sharedId), userKeys.session, userKeys.firstName, userKeys.lastName);
		}
	}).then(function(values) {
		if (!values[0]) {
			values[0] = utils.generateSessionToken();
		}

		return redis
			.hmsetAsync(userKeyFromId(sharedId), userKeys.session, values[0], userKeys.active, true)
			.thenReturn(values);
	}).then(function(values) {
		return updatePreKeys(sharedId, preKeys)
			.thenReturn(values);
	}).then(function(values) {
		cb(null, sharedId, values[0], values[1], values[2]);
	}, function(err) {
		cb(err);
	});
};

function updatePreKeys(userId, preKeys) {
	var indices = preKeys['i'];
	var publicKeys = preKeys['pk'];
	if (indices.length != publicKeys.length) {
		return Promise.reject("prekeys");
	}

	var keyValues = [];
	for (var i = 0; i < indices.length; i++) {
		keyValues.push(indices[i]);
		keyValues.push(publicKeys[i]);
	}

	return redis.multi()
		.hmset(preKeysKeyFromId(userId), keyValues)
		.sadd(preKeyIndicesKeyFromId(userId), indices.filter(function(index) { return index != 0xFFFF; }))
		.exec().then(function(values) {
			if (values.length == 2 && values[0][1] == 'OK') {
				return Promise.resolve(true);
			} else {
				return Promise.reject("prekeys");
			}
		});
};
exports.updatePreKeys = updatePreKeys;

// Returns a promise.
exports.exists = function(id) {
	return redis.existsAsync(userKeyFromId(id));
};

exports.confirmSession = function(id, sessionToken, cb) {
	redis.hgetAsync(userKeyFromId(id), userKeys.session).then(function(dbSessionToken) {
		cb(null, sessionToken && dbSessionToken == sessionToken);
	}, function(err) {
		cb(err);
	});
};

exports.checkUsersWithPhoneNumbers = function(phoneNumbers, cb) {
	var sharedIds;

	var promises = [];
	for (var i = 0, phoneNumber; phoneNumber = phoneNumbers[i]; i++) {
		promises.push(userIdFromPhoneNumber(phoneNumber));
	}
	Promise.all(promises).then(function(ids) {
		sharedIds = ids;

		var infoPromises = [];
		for (var i = 0; i < ids.length; i++) {
			var id = ids[i];
			if (id) {
				infoPromises.push(getUserInfo(ids[i], ['key']));
			} else {
				infoPromises.push(Promise.resolve([null]));
			}
		}
		return Promise.all(infoPromises);
	}).then(function(allInfo) {
		var results = [];
		for (var i = 0; i < allInfo.length; i++) {
			if (sharedIds[i] && allInfo[i]) {
				results.push({
					'id': sharedIds[i],
					'key': allInfo[i][0],
					'phone': phoneNumbers[i]
				});
			} else {
				results.push(null);
			}
		}
		cb(null, results);
	}, function(err) {
		cb(err);
	});
}

exports.setDeviceToken = function(userId, token, cb) {
	redis.hsetAsync(userKeyFromId(userId), userKeys.iosPushToken, token).then(function() {
		cb();
	}, function(err) {
		cb(err);
	});
};

exports.updateInfo = function(userId, firstName, lastName, cb) {
	if (!firstName) {
		cb('firstName required');
		return;
	}

	redis.hmsetAsync(userKeyFromId(userId), userKeys.firstName, firstName, userKeys.lastName || '', lastName).then(function() {
		cb();
	}, function(err) {
		cb(err);
	});
};