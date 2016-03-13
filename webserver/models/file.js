var redis = require('./redis').redis,
	user = require('./user').User,
	Promise = require('bluebird').Promise,
	s3 = require('../utils/s3'),
	User = require('./user').User;

const fileIdKey = 'file_id';

var File = function(id) {
	this.id = id;
};
exports.File = File;

File.create = function(user, friend, numIds) {
	return friend.exists().then(function(exists) {
		if (exists) {
			return redis.incrbyAsync(fileIdKey, numIds);
		} else {
			return Promise.reject();
		}
	}).then(function(fileId) {
		var firstFileId = fileId - numIds;

		var promises = [];
		for (var i = 0; i < numIds; i++) {
			promises.push(redis.saddAsync(File._fileKey(firstFileId + i), File._userKey(user.id), File._userKey(friend.id)));
		}
		
		console.log(firstFileId);
		console.log(firstFileId + numIds);

		var files = [];
		for (var fileId = firstFileId; fileId < firstFileId + numIds; fileId++) {
			files.push(new File(fileId));
		}
		
		return Promise.all(promises).thenReturn(files);
	});
};

File._fileKey = function(fileId) {
	return 'f:{' + fileId + '}';
};

File._userKey = function(userId) {
	return 'u:' + userId;
};

File.prototype.hasAccess = function(user) {
	return redis.sismemberAsync(File._fileKey(this.id), File._userKey(user.id))
};

File.prototype.generateSignedUrl = function(fileId, method, contentType) {
	return s3.generateSignedUrl(method, 'files/' + fileId, 'drtyhbo-chat', contentType);
};