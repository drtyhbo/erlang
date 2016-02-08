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

exports.redis = new Redis(6379, 'chat-redis');