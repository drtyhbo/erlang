
var redis = require('../models/redis').redis;

exports.deleteUser = function(phoneNumber) {
	var phoneNumberKey = 'p:{' + phoneNumber + '}'
	return redis.getAsync(phoneNumberKey).then(function(value) {
		if (value) {
			return redis
				.multi()
				.del(phoneNumberKey)
				.del('u:{' + value + '}')
				.exec();
		} else {
			return Promise.resolve();
		}
	});
}