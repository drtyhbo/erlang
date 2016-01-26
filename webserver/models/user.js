var client = require('./redis').client;

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