var express = require('express'),
	Device = require('../models/device').Device,
	User = require('../models/user').User,
	File = require('../models/file').File,
	s3 = require('../utils/s3');

var router = express.Router();

router.use(function(req, res, next) {
	var deviceId = req.body.deviceId;
	var sessionToken = req.body.session;

	var device = new Device(deviceId);
	device.confirmSession(sessionToken).then(function(user) {
		req.user = user;
		req.device = device;
		next();
	}, function(err) {
		res.send({ 'status': 'session' });
	});
});

function sendError(res) {
	res.send({ 'status': 'error' });
}

function sendSuccess(res, result) {
	result = result || {};
	result['status'] = 'ok';
	res.send(result);
}

/*
 * Request parameters:
 * phone - The phone numbers to check.
 */
router.post('/friend/check/', function(req, res) {
	var phone = req.body.phone;
	if (!(phone instanceof Array)) {
		phone = [phone];
	}

	User.checkPhoneNumbers(phone).then(function(users) {
		res.send({
			'status': 'ok',
			'friends': users
		});
	}, function() {
		sendError(res);
	});
});

/*
 * Request parameters:
 * userId - Id of the user for whom we need a prekey.
 */
router.post('/friend/prekey/', function(req, res) {
	var userId = req.body.userId;
	if (!userId) {
		sendError(res);
		return;
	}

	new User(userId).fetchPreKey().then(function(key) {
		res.send({
			'status': 'ok',
			'keyIndex': key.index,
			'publicKey': key.key
		});
	}, function() {
		sendError(res);
	});
});

/*
 * Request parameters:
 * token - The device token.
 */
router.post('/pns/register/', function(req, res) {
	var token = req.body.token;
	if (!token) {
		sendError(res);
		return;
	}

	req.device.registerPnsToken(token).then(function() {
		res.send({ 'status': 'ok' })
	}, function() {
		sendError(res);
	});
});

/*
 * Request parameters:
 * friendId - The friend whom should have access.
 */
router.post('/file/create/', function(req, res) {
	var numIds = parseInt(req.body.numIds, 10);
	if (!numIds || numIds < 0 || !req.body.friendId) {
		sendError(res);
		return;
	}

	File.create(req.user, new User(req.body.friendId), numIds).then(function(files) {
		res.send({
			'status': 'ok',
			'fileIds': files.map(function(file) { return file.id }) 
		});
	}, function() {
		sendError(res);
	});
});

/*
 * Request parameters:
 * fileId - The id of the file.
 */
router.post('/file/get/', function(req, res) {
	var fileId = req.body.fileId;
	var method = req.body.method;
	var contentType = req.body.contentType || '';

	if (!fileId || !method) {
		sendError(res);
		return
	}

	var file = new File(fileId);
	file.hasAccess(req.user).then(function(isMember) {
		var result = {
			'status': 'ok'
		};
		if (isMember) {
			result['fileUrl'] = file.generateSignedUrl(fileId, method.toUpperCase(), contentType);
		}

		res.send(result);
	}, function() {
		sendError(res);
	});
});

/*
 *
 */
router.post('/profilepic/', function(req, res) {
	res.send({
		'status': 'ok',
		'uploadUrl': s3.generateSignedUrl('PUT', req.user.id, 'drtyhbo-chat-users', 'image/jpeg')
	});
});

/*
 * Request parameters:
 * firstName - The user's first name.
 * lastName - The user's last name.
 */
router.post('/info/update/', function(req, res) {
	var firstName = (req.body.firstName || '').trim();
	var lastName = (req.body.lastName || '').trim();

	var fieldsToUpdate = [];
	if (firstName) {
		fieldsToUpdate = fieldsToUpdate.concat([User.fields.firstName, firstName]);
	}
	if (lastName) {
		fieldsToUpdate = fieldsToUpdate.concat([User.fields.lastName, lastName]);
	}

	if (!fieldsToUpdate.length) {
		sendSuccess(res);
	} else {
		req.user.update(fieldsToUpdate).then(function() {
			sendSuccess(res);
		}, function() {
			sendError(res);
		});
	}
});

/*
 * Request parameters:
 * userIds - The ids of the users.
 */
router.post('/info/get/', function(req, res) {
	var userIds = (req.body.userIds || []);
	
	if (!userIds) {
		sendError(res);
		return;
	}

	var users = userIds.map(function(id) { return new User(id); });
	var promises = [];
	for (var i = 0, user; user = users[i]; i++) {
		promises.push(user.fetch(User.fields.firstName, User.fields.lastName));
	}

	Promise.all(promises).then(function(info) {
		var names = [];
		for (var i = 0, name; name = info[i]; i++) {
			if (name[0] && name[1]) {
				names.push({
					firstName: name[0],
					lastName: name[1]
				});
			} else {
				names.push(null);
			}
		}
		sendSuccess(res, {
			names: names
		});
	});
});

module.exports = router;