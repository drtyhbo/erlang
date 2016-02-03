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
				infoPromises.push(getUserInfo(ids[i], ['name']));
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
					'name': allInfo[i][0]
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