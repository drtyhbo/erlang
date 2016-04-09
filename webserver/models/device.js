var mongoose = require('mongoose'),
	utils = require('../utils/utils.js'),
	Promise = require('bluebird').Promise;

var deviceSchema = mongoose.Schema({
	deviceUuid: String,
	code: String,
	iosPushToken: String,
	session: String,
	userId: mongoose.Schema.Types.ObjectId,
	preKeys: [mongoose.Schema.Types.Mixed]
});

deviceSchema.methods.confirmSession = function(session) {
	return session && this.session && session == this.session;
};

deviceSchema.methods.fetchPreKey = function() {
	var preKey = {
		index: this.preKeys[0].i,
		key: this.preKeys[0].pk
	};

	if (preKey.index != Device.keyOfLastResort) {
		return this.update({ $pull: { preKeys: { i: preKey.index }}}).then(function() {
			return Promise.resolve(preKey);
		})
	} else {
		return Promise.resolve(preKey);
	}
};

deviceSchema.methods.generateCode = function() {
	if (this.code) {
		return Promise.resolve(this.code);
	}

	var code = Math.floor(Math.random() * 900000) + 100000;
	this.code = code;
	return this.save().then(function(user) {
		return Promise.resolve(code);
	});
};

deviceSchema.methods.login = function() {
	var sessionToken = utils.generateSessionToken();

	this.session = sessionToken
	return this.save().then(function() {
		return Promise.resolve(sessionToken);
	});
};

deviceSchema.methods.registerPnsToken = function(pnsToken) {
	this.iosPushToken = pnsToken;
	return this.save();
};

deviceSchema.methods.updatePreKeys = function(preKeys) {
	var indices = preKeys['i'];
	var publicKeys = preKeys['pk'];
	if (indices.length != publicKeys.length) {
		return Promise.reject();
	}

	for (var i = 0; i < indices.length; i++) {
		this._updatePreKey(indices[i], publicKeys[i]);
	}

	return this.save();
};

deviceSchema.methods._updatePreKey = function(index, publicKey) {
	for (var i = 0; i < this.preKeys.length; i++) {
		if (this.preKeys[i].i == index) {
			this.preKeys[i].pk = publicKey;
			return
		}
	}

	this.preKeys.push({
		i: index,
		pk: publicKey
	});
};

var Device = mongoose.model('Device', deviceSchema);

Device.keyOfLastResort = 0xFFFF;

Device.create = function(deviceUuid, userId) {
	if (!deviceUuid || !userId || !Device._verifyUuid(deviceUuid)) {
		return Promise.reject();
	}

	return Device.find({
		deviceUuid: deviceUuid,
		userId: userId
	}).then(function(devices) {
		if (!devices.length) {
			var device = new Device({
				deviceUuid: deviceUuid,
				userId: userId
			});
			return device.save();
		} else {
			return Promise.resolve(devices[0]);
		}
	});
};

Device.findDevice = function(deviceUuid) {
	if (!deviceUuid) {
		return Promise.reject();
	}

	return Device.find({ deviceUuid: deviceUuid }).then(Device._findDeviceCallback);
};

Device.findDeviceById = function(deviceId) {
	if (!deviceId) {
		return Promise.reject();
	}

	return Device.find({ _id: deviceId }).then(Device._findDeviceCallback);
};

Device._findDeviceCallback = function(devices) {
	if (!devices.length) {
		return Promise.reject();
	} else {
		return Promise.resolve(devices[0]);
	}
}

Device._verifyUuid = function(uuid) {
	return new RegExp('[a-f0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}', 'i').test(uuid);
};

exports.Device = Device;
/*Device.prototype.confirmSession = function(sessionToken) {
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
};*/
