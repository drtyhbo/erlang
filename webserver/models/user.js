var client = require('./redis').client,
	utils = require('../utils/utils.js'),
	Promise = require('bluebird').Promise;

function userKeyFromId(id) {
	return 'user_' + id;
}

function createNewUser(phoneNumber) {
	var code = Math.floor(Math.random() * 900000) + 100000;
	return client.incrAsync('user_id').then(function(id) {
		return client.setAsync(phoneNumber, id).thenReturn(id);
	}).then(function(id) {
		return client.hmsetAsync(userKeyFromId(id), ['phoneNumber', phoneNumber]).thenReturn(id);
	});
}

exports.create = function(phoneNumber, cb) {
	var userId;
	client.getAsync(phoneNumber).then(function(id) {
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
		console.log()
		cb(err, null);
	});
};

exports.login = function(phoneNumber, code, cb) {
	var sharedId;
	client.getAsync(phoneNumber).then(function(id) {
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