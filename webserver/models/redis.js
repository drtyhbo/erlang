var redis = require('redis'),
	bluebird = require('bluebird');

bluebird.promisifyAll(redis.RedisClient.prototype);

exports.client = redis.createClient();