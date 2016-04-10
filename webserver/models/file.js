var mongoose = require('mongoose'),
	s3 = require('../utils/s3'),
	utils = require('../utils/utils.js'),
	Promise = require('bluebird').Promise;

var fileSchema = mongoose.Schema({
	userIds: [mongoose.Schema.Types.ObjectId]
});

fileSchema.methods.hasAccess = function(user) {
	return this.userIds.indexOf(user._id) != -1;
};

fileSchema.methods.generateSignedUrl = function(method, contentType) {
	return s3.generateSignedUrl(method, 'files/' + this._id, 'drtyhbo-chat', contentType);
};

var File = mongoose.model('File', fileSchema);

File.create = function(users, numFiles) {
	var promises = [];

	var userIds = users.map(function(user) { return user._id });
	for (var i = 0; i < numFiles; i++) {
		var file = new File({ userIds: userIds });
		promises.push(file.save());
	}

	return Promise.all(promises);
};

File.findById = function(fileId) {
	return File.find({ _id: fileId }).then(function(files) {
		if (!files.length) {
			return Promise.reject();
		} else {
			return Promise.resolve(files[0]);
		}
	});
};

exports.File = File;
