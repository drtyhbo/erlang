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

Device._deviceKey = function(id) {
	return 'd:{' + id + '}';
};

Device.prototype._update = function() {
	return redis.hmsetAsync(Device._deviceKey(this.id), Array.prototype.slice.call(arguments));
};