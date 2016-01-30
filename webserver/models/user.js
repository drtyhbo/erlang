var client = require('./redis').client,
	utils = require('../utils/utils.js');

exports.create = function(phoneNumber, cb) {
	client.hget(phoneNumber, 'code', function(err, code) {
		if (!code || err) {
			code = Math.floor(Math.random() * 900000) + 100000;
			client.hset(phoneNumber, 'code', code, function(err, code) {
				if (err) cb(err);
				else cb(null, code);
			});
		} else {
			cb(null, code);
		}
	});
};

exports.login = function(phoneNumber, code, cb) {
	client.hget(phoneNumber, 'code', function(err, retrievedCode) {
		if (!code || retrievedCode != code || err) {
			cb("Codes don't match");
		} else {
			client.hget(phoneNumber, 'session', function(err, sessionToken) {
				if (sessionToken) {
					cb(null, sessionToken);
					return
				}

				sessionToken = utils.generateSessionToken();
				client.hset(phoneNumber, 'session', sessionToken, function(err, response) {
					cb(err, response == 1 ? sessionToken : null);
				});
			});
		}
	});
};