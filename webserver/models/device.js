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
		this._updatePreKey(parseInt(indices[i], 10), publicKeys[i]);
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

Device.findDevicesForUser = function(user) {
	return Device.find({ userId: user._id });
};

Device._findDeviceCallback = function(devices) {
	if (!devices.length) {
		return Promise.reject();
	} else {
		return Promise.resolve(devices[0]);
	}
}

Device._verifyUuid = function(uuid) {
	return new RegExp('[a-f0-9]{32}', 'i').test(uuid);
};

exports.Device = Device;
