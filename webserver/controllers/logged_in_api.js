var express = require('express'),
	File = require('../models/file').File,
	s3 = require('../utils/s3'),
	Device = require('../models/device').Device,
	Promise = require('bluebird').Promise,
	User = require('../models/user').User;


var router = express.Router();


router.use(function(req, res, next) {
	var deviceId = req.body.id;
	var session = req.body.session;

	Device.findDeviceById(deviceId).then(function(device) {
		if (!device.confirmSession(session)) {
			return Promise.reject();
		}

		req.device = device;
		return User.findUserById(device.userId)
	}).then(function(user) {
		req.user = user;
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
			'friends': users.map(function(user) {
				if (!user) {
					return null;
				}
				return {
					phone: user.phone,
					id: user._id
				};
			})
		});
	}, function() {
		sendError(res);
	});
});

/*
 * Request parameters:
 * phone - The phone numbers to check.
 */
router.post('/device/active/', function(req, res) {
	var userIds = req.body.userIds;
	if (!userIds) {
		sendError(res);
		return;
	}

	User.findUsersByIds(userIds).then(function(users) {
		var promises = [];
		for (var i = 0, user; user = users[i]; i++) {
			promises.push(user.getActiveDevice());
		}
		return Promise.all(promises);
	}).then(function(devices) {
		sendSuccess(res, {
			'deviceIds': devices.map(function(device) { if (device) return device._id; })
		});
	});
});

/*
 * Request parameters:
 * userId - Id of the user for whom we need a prekey.
 */
router.post('/device/prekey/', function(req, res) {
	var deviceId = req.body.deviceId;
	if (!deviceId) {
		sendError(res);
		return;
	}

	Device.findDeviceById(deviceId).then(function(device) {
		return device.fetchPreKey();
	}).then(function(key) {
		res.send({
			'status': 'ok',
			'keyIndex': key.index,
			'publicKey': key.key
		});
	}, function(err) {
		console.log(err);
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
	var friendIds = req.body.friendIds;
	if (!numIds || numIds <= 0 || !friendIds) {
		sendError(res);
		return;
	}

	var promises = [];
	for (var i = 0, friendId; friendId < friendIds.length; i++) {
		promises.push(User.findUserById(friendId));
	}

	Promise.all(promises).then(function(users) {
		users.push(req.user);
		return File.create(users, numIds);
	}).then(function(files) {
		res.send({
			'status': 'ok',
			'fileIds': files.map(function(file) { return file._id })
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

	File.findById(fileId).then(function(file) {
		var result = {
			'status': 'ok'
		};
		if (file.hasAccess(req.user)) {
			result['fileUrl'] = file.generateSignedUrl(method.toUpperCase(), contentType);
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
		'uploadUrl': s3.generateSignedUrl('PUT', req.user._id, 'drtyhbo-chat-users', 'image/jpeg')
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
	if (!firstName) {
		sendSuccess(res);
		return
	}

	req.user.first = firstName;
	req.user.last = lastName;
	req.user.save().then(function() {
		sendSuccess(res);
	}, function() {
		sendError(res);
	});
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

	var promises = [];
	for (var i = 0, userId; userId = userIds[i]; i++) {
		promises.push(User.findById(userId));
	}

	Promise.all(promises).then(function(users) {
		var names = [];
		for (var i = 0, user; user = users[i]; i++) {
			if (user.first) {
				names.push({
					firstName: user.first || '',
					lastName: user.last || ''
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
