var redis = require('./redis').redis,
	user = require('./user'),
	Promise = require('bluebird').Promise,
	s3 = require('../utils/s3');

function fileIdKey() {
	return "file_id";
}

function fileKeyFromId(fileId) {
	return 'f:{' + fileId + '}';
}

function userKeyFromId(userId) {
	return 'u:' + userId;
}

exports.create = function(userId, friendId, numIds, cb) {
	user.exists(friendId).then(function(exists) {
		if (exists) {
			return redis.incrbyAsync(fileIdKey(), numIds);
		} else {
			return Promise.reject();
		}
	}).then(function(fileId) {
		var firstFileId = fileId - numIds;

		var promises = [];
		for (var i = 0; i < numIds; i++) {
			promises.push(redis.saddAsync(fileKeyFromId(firstFileId + i), userKeyFromId(userId), userKeyFromId(friendId)));
		}
		
		return Promise.all(promises).thenReturn(firstFileId);
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
	return s3.generateSignedUrl(method, 'files/' + fileId, 'drtyhbo-chat', contentType);
}