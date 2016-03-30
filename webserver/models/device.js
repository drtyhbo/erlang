var redis = require('./redis').redis,
	utils = require('../utils/utils.js'),
	Promise = require('bluebird').Promise;

const deviceIdKey = 'device_id';

var Device = function(id) {
	this.id = id;
};
exports.Device = Device;

Device.fields = {
	code: 'code',
	iosPushToken: 'iosToken',
	userId: 'u',
	session: 'session'
};

Device.keyOfLastResort = 0xFFFF;

Device.create = function(userId) {
	return redis.incrAsync(deviceIdKey).then(function(id) {
		var device = new Device(id);
		return device._update(Device.fields.userId, userId).thenReturn(device);
	});
};

Device.prototype.confirmSession = function(sessionToken) {
	var User = require('../models/user').User;

	if (!sessionToken) {
		return Promise.reject();
	}

	var self = this;
	return this.fetch(Device.fields.session, Device.fields.userId).then(function(values) {
		if (sessionToken == values[0] && values[1]) {
			return Promise.resolve(new User(values[1]));
		} else {
			return Promise.reject();
		}
	});
};

Device.prototype.fetch = function() {
	return redis.hmgetAsync(Device._deviceKey(this.id), Array.prototype.slice.call(arguments));
};

Device.prototype.fetchPreKey = function() {
	var self = this;

	var sharedKeyIndex;
	return redis.spopAsync(Device._preKeyIndicesKey(this.id)).then(function(keyIndex) {
		if (keyIndex === null) {
			keyIndex = Device.keyOfLastResort;
		}
		
		sharedKeyIndex = keyIndex;
		var multi = redis
			.multi()
			.hget(Device._preKeysKey(self.id), keyIndex);		

		if (keyIndex != Device.keyOfLastResort) {
			multi.hdel(Device._preKeysKey(self.id), keyIndex)
		}

		return multi.exec();
	}).then(function(values) {
		if (!values[0][1]) {
			return Promise.reject();
		} else {
			return Promise.resolve({
				index: parseInt(sharedKeyIndex, 10),
				key: values[0][1]
			});
		}
	});
};

Device.prototype.generateCode = function() {
	var self = this;
	return this.fetch(Device.fields.code).then(function(values) {
		var code = values[0]
		if (!code) {
			code = Math.floor(Math.random() * 900000) + 100000;;
			return self._update(Device.fields.code, code).thenReturn(code);
		} else {
			return Promise.resolve(code);
		}
	});
}

Device.prototype.login = function() {
	var sessionToken = utils.generateSessionToken();
	return this._update(Device.fields.session, sessionToken).thenReturn(sessionToken);
};

Device.prototype.registerPnsToken = function(pnsToken) {
	return this._update(Device.fields.iosPushToken, pnsToken);
};

Device.prototype.updatePreKeys = function(preKeys) {
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
		.hmset(Device._preKeysKey(this.id), keyValues)
		.sadd(Device._preKeyIndicesKey(this.id), indices.filter(function(index) { return index != Device.keyOfLastResort; }))
		.exec()
		.then(function(values) {
			if (values.length == 2 && values[0][1] == 'OK') {
				return Promise.resolve(true);
			} else {
				return Promise.reject();
			}
		});
};

Device._deviceKey = function(id) {
	return 'd:{' + id + '}';
};

Device._preKeysKey = function(id) {
	return 'pk:{' + id + '}';
};

Device._preKeyIndicesKey = function(id) {
	return 'pki:{' + id + '}';
};

Device.prototype._update = function() {
	return redis.hmsetAsync(Device._deviceKey(this.id), Array.prototype.slice.call(arguments));
};