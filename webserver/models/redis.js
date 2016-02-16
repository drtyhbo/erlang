var Redis = require('ioredis');
var bluebird = require('bluebird');

bluebird.promisifyAll(Redis.prototype);
bluebird.promisifyAll(Redis.Cluster.prototype);

/*exports.redis = new Redis.Cluster([{
	port: 30001,
	host: '127.0.0.1'
}, {
	port: 30002,
	host: '127.0.0.1'
}]);*/

var redis = new Redis({
	port: 6379,
	host: 'redis1',
	retryStrategy: function (times) {
		return Math.random() * times * 1000;
	}
});

redis.on('connect', function() {
	console.log("**** Connected to redis ****");
}).on('close', function() {
	console.log("**** Disconnection from redis ****");
}).on('reconnecting', function() {
	console.log("**** Reconnecting to redis ****");
});

exports.redis = redis;