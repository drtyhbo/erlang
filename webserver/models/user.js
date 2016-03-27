var redis = require('./redis').redis,
	utils = require('../utils/utils.js'),
	Device = require('../models/device.js').Device,
	Promise = require('bluebird').Promise;

var User = function(id) {
	this.id = id;
};
exports.User = User;

User.fields = {
	phone: 'phone',
	code: 'code',
	active: 'active',
	key: 'key',
	firstName: 'firstName',
	lastName: 'lastName'
};

User.keyOfLastResort = 0xFFFF;

// Checks whether the provided numbers correspond to users and returns an array of the
// phoneNumber/id combinations.
User.checkPhoneNumbers = function(phoneNumbers) {
	var sharedIds;

	var promises = [];
	for (var i = 0, phoneNumber; phoneNumber = phoneNumbers[i]; i++) {
		promises.push(this._getUserId(phoneNumber));
	}

	return Promise.all(promises).then(function(ids) {
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
		return Promise.resolve(results);
	});
};

// Resolves to an array with the following members:
// 0 - The user object.
// 1 - The device object.
// 2 - The confirmation code.
User.create = function(phoneNumber, deviceUuid) {
	if (!phoneNumber || phoneNumber.length != 11) {
		return Promise.reject();
	}

	var sharedUser;
	var sharedDevice;
	return this._getUserId(phoneNumber).then(function(id) {
		if (!id) {
			return User._create(phoneNumber);
		} else {
			return Promise.resolve(new User(id));
		}
	}).then(function(user) {
		sharedUser = user;
		return user.findDevice(deviceUuid);
	}).then(function(device) {
		sharedDevice = device;
		return device.generateCode();
	}).then(function(code) {
		return Promise.resolve([sharedUser, sharedDevice, code]);
	});
};

// Resolves to an array with the following members:
// 0 - The user object.
// 1 - The device object.
User.verifyNumber = function(phoneNumber, deviceUuid, code) {
	var sharedUser;
	var sharedDevice;
	return User._getUserId(phoneNumber).then(function(id) {
		if (!id) {
			return Promise.reject();
		}

		sharedUser = new User(id);
		return sharedUser.findDevice(deviceUuid);
	}).then(function(device) {
		sharedDevice = device;
		return device.fetch(Device.fields.code);
	}).then(function(values) {
		if (!code || values[0] != code) {
			return Promise.reject();
		}
		return sharedUser
			._update(User.fields.active, true)
			.thenReturn([sharedUser, sharedDevice]);
	});
};

User._create = function(phoneNumber) {
	//var code = Math.round(Math.random() * 899999) + 100000;
	var code = 111111;
	return redis.incrAsync('user_id').then(function(id) {
		return redis.setAsync(User._phoneKey(phoneNumber), id).thenReturn(id);
	}).then(function(id) {
		return redis.hmsetAsync(User._userKey(id), [User.fields.phone, phoneNumber]).thenReturn(new User(id));
	});
}

User._getUserId = function(phoneNumber) {
	return redis.getAsync(User._phoneKey(phoneNumber));
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

User._userKey = function(id) {
	return 'u:{' + id + '}';
};

User.prototype.exists = function() {
	return redis.existsAsync(User._userKey(this.id));
};

User.prototype.fetch = function() {
	return redis.hmgetAsync(User._userKey(this.id), Array.prototype.slice.call(arguments));
};

// Returns the next preKey, or the key of last resort if none are available. Returns a rejection
// promise if no keys can be found.
User.prototype.fetchPreKey = function() {
	var self = this;

	var sharedKeyIndex;
	return redis.spopAsync(User._preKeyIndicesKey(this.id)).then(function(keyIndex) {
		if (keyIndex === null) {
			keyIndex = User.keyOfLastResort;
		}
		
		sharedKeyIndex = keyIndex;
		var multi = redis
			.multi()
			.hget(User._preKeysKey(self.id), keyIndex);		

		if (keyIndex != User.keyOfLastResort) {
			multi.hdel(User._preKeysKey(self.id), keyIndex)
		}

		return multi.exec();
	}).then(function(values) {
		if (!values[0][1]) {
			return Promise.reject();
		} else {
			return Promise.resolve({
				index:  parseInt(sharedKeyIndex, 10),
				key: values[0][1]
			});
		}
	});
};

User.prototype.findDevice = function(deviceUuid) {
	var self = this;
	return redis.hgetAsync(User._userKey(this.id), 'd:' + deviceUuid).then(function(deviceId) {
		if (deviceId) {
			return Promise.resolve(deviceId ? new Device(deviceId) : null);
		} else {
			return self._createDevice(deviceUuid);
		}
	});
};


User.prototype.update = function() {
	if (arguments[0] instanceof Array) {
		var itemsToUpdate = arguments[0];
	} else {
		var itemsToUpdate = Array.prototype.slice.call(arguments);
	}
	
	for (var i = itemsToUpdate.length - 2; i >= 0; i -= 2) {
		var fieldName = itemsToUpdate[i];
		if (fieldName != User.fields.firstName && fieldName != User.fields.lastName && fieldName != User.fields.iosPushToken) {
			itemsToUpdate.splice(i, 2);
		}
	}
	
	return this._update.apply(this, itemsToUpdate);
};

// Updates the user's pre-key cache with the provided pre-keys.
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

User.prototype._createDevice = function(deviceUuid) {
	var self = this;
	return Device.create(this.id).then(function(device) {
		return redis.hsetAsync(User._userKey(self.id), 'd:' + deviceUuid, device.id).thenReturn(device);
	});
};

User.prototype._update = function() {
	return redis.hmsetAsync(User._userKey(this.id), Array.prototype.slice.call(arguments));
};