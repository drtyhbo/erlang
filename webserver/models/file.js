var redis = require('./redis').redis,
	user = require('./user'),
	sig = require('amazon-s3-url-signer'),
	Promise = require('bluebird').Promise;

var fileBucket = sig.urlSigner('AKIAIRKB5XME5BRBKYWQ', 'mC2r6bNkM6AaxWqBO6iJ3enL6yACsbOjOEvWrwYv', {
	useSubdomain: true,
	protocol: "https"});

function fileIdKey() {
	return "file_id";
}

function fileKeyFromId(fileId) {
	return 'f:{' + fileId + '}';
}

function userKeyFromId(userId) {
	return 'u:' + userId;
}

exports.create = function(userId, friendId, cb) {
	user.exists(friendId).then(function(exists) {
		if (exists) {
			return redis.incrAsync(fileIdKey());
		} else {
			return Promise.reject();
		}
	}).then(function(fileId) {
		return redis.saddAsync(fileKeyFromId(fileId), userKeyFromId(userId), userKeyFromId(friendId)).thenReturn(fileId);
	}).then(function(fileId) {
		cb(null, fileId);
	}, function(err) {
		cb('error');
	})
};

exports.hasAccess = function(fileId, userId, cb) {
	return redis.sismemberAsync(fileKeyFromId(fileId), userKeyFromId(userId)).then(function(isMember) {
		cb(null, isMember);
	}, function(err) {
		cb('error');
	})
};

exports.generateSignedUrl = function(fileId, method, contentType) {
	return fileBucket.getUrl(method, 'files/' + fileId, 'drtyhbo-chat', contentType, 100);
}