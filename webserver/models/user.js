var redis = require('./redis').redis,
	utils = require('../utils/utils.js'),
	Promise = require('bluebird').Promise;

var User = function(id) {
	this.id = id;
};

User.fields = {
	phone: 'phone',
	code: 'code',
	session: 'session',
	active: 'active',
	key: 'key',
	iosPushToken: 'iosToken',
	firstName: 'firstName',
	lastName: 'lastName'
};

User.keyOfLastResort = 0xFFFF;

User._userKey = function(id) {
	return 'u:{' + id + '}';
};

User._phoneKey = function(phoneNumber) {
	return 'p:{' + phoneNumber + '}';
};

User._preKeysKey = function(id) {
	return 'pk:{' + id + '}';
};

User._preKeyIndicesKey = function(id) {
	return 'pki:{' + id + '}';
};

User._getUserId = function(phoneNumber) {
	return redis.getAsync(User._phoneKey(phoneNumber));
};

// Creates a new user. Returns a promise that resolves to the user.
// Has tests.
User.create = function(phoneNumber) {
	if (!phoneNumber || phoneNumber.length != 11) {
		return Promise.reject();
	}

	var sharedId;
	return this._getUserId(phoneNumber).then(function(id) {
		if (!id) {
			return User._create(phoneNumber);
		} else {
			return Promise.resolve(new User(id));
		}
	}).then(function(user) {
		return user._generateCode().thenReturn(user);
	}).then(function(user) {
		return Promise.resolve(user);
	});
};

User.prototype._generateCode = function() {
	var self = this;
	return redis.hgetAsync(User._userKey(this.id), User.fields.code).then(function(code) {
		if (!code) {
			self._code = Math.floor(Math.random() * 900000) + 100000;;
			return redis.hsetAsync(User._userKey(self.id), User.fields.code, self._code).thenReturn(self._code);
		} else {
			return Promise.resolve(code);
		}
	});
}

// Returns a session key for the user (assuming the codes match).
// Has tests.
User.login = function(phoneNumber, code) {
	var self = this;

	var sharedUser;
	return this._getUserId(phoneNumber).then(function(id) {
		if (!id) {
			return Promise.reject();
		}

		sharedUser = new User(id);
		return sharedUser.fetch(User.fields.code);
	}).then(function(values) {
		if (!code || values[0] != code) {
			return Promise.reject();
		}
		return sharedUser.fetch(User.fields.session);
	}).then(function(values) {
		if (!values[0]) {
			values[0] = utils.generateSessionToken();
		}
		return sharedUser
			._update(User.fields.session, values[0], User.fields.active, true)
			.thenReturn(sharedUser);
	});
};

User._create = function(phoneNumber) {
	var code = Math.floor(Math.random() * 900000) + 100000;
	return redis.incrAsync('user_id').then(function(id) {
		return redis.setAsync(User._phoneKey(phoneNumber), id).thenReturn(id);
	}).then(function(id) {
		return redis.hmsetAsync(User._userKey(id), [userKeys.phone, phoneNumber]).thenReturn(new User(id));
	});
}

// Has tests.
User.prototype.fetch = function() {
	return redis.hmgetAsync(User._userKey(this.id), Array.prototype.slice.call(arguments));
};

// Has tests.
User.prototype.update = function() {
	var itemsToUpdate = Array.prototype.slice.call(arguments);
	for (var i = itemsToUpdate.length - 2; i >= 0; i -= 2) {
		var fieldName = itemsToUpdate[i];
		if (fieldName != User.fields.firstName && fieldName != User.fields.lastName && fieldName != User.fields.iosPushToken) {
			itemsToUpdate.splice(i, 2);
		}
	}
	return this._update.apply(this, itemsToUpdate);
};

User.prototype._update = function() {
	return redis.hmsetAsync(User._userKey(this.id), Array.prototype.slice.call(arguments));
};

// Updates the user's pre-key cache with the provided pre-keys.
// Has tests.
User.prototype.updatePreKeys = function(preKeys) {
	var indices = preKeys['i'];
	var publicKeys = preKeys['pk'];

	if (indices.length != publicKeys.length) {
		return Promise.reject();
	}

	var keyValues = [];
	for (var i = 0; i < indices.length; i++) {
		keyValues.push(indices[i]);
		keyValues.push(publicKeys[i]);
	}

	return redis
		.multi()
		.hmset(User._preKeysKey(this.id), keyValues)
		.sadd(User._preKeyIndicesKey(this.id), indices.filter(function(index) { return index != User.keyOfLastResort; }))
		.exec()
		.then(function(values) {
			if (values.length == 2 && values[0][1] == 'OK') {
				return Promise.resolve(true);
			} else {
				return Promise.reject();
			}
		});
};

exports.User = User;

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

const keyOfLastResort = 0xFFFF;

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
		.sadd(preKeyIndicesKeyFromId(userId), indices.filter(function(index) { return index != keyOfLastResort; }))
		.exec().then(function(values) {
			if (values.length == 2 && values[0][1] == 'OK') {
				return Promise.resolve(true);
			} else {
				return Promise.reject("prekeys");
			}
		});
};
exports.updatePreKeys = updatePreKeys;

exports.getPreKey = function(userId, cb) {
	var sharedKeyIndex;
	redis.spopAsync(preKeyIndicesKeyFromId(userId)).then(function(keyIndex) {
		if (keyIndex === null) {
			keyIndex = keyOfLastResort;
		}
		sharedKeyIndex = keyIndex;

		var multi = redis
			.multi()
			.hget(preKeysKeyFromId(userId), keyIndex);		

		if (keyIndex != keyOfLastResort) {
			multi.hdel(preKeysKeyFromId(userId), keyIndex)
		}

		return multi.exec();
	}).then(function(values) {
		cb(values[0][1], parseInt(sharedKeyIndex, 10));
	});
};

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
		var results = [];
		for (var i = 0; i < ids.length; i++) {
			if (ids[i]) {
				results.push({
					'id': ids[i],
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