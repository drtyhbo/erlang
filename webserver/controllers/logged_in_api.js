var express = require('express'),
	User = require('../models/user').User,
	File = require('../models/file').File,
	Group = require('../models/group').Group,
	s3 = require('../utils/s3');

var router = express.Router();

router.use(function(req, res, next) {
	var id = req.body.id;
	var sessionToken = req.body.session;

	new User(id).confirmSession(sessionToken).then(function(user) {
		req.user = user;
		req.sessionToken = sessionToken;
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

	req.user.update(User.fields.iosPushToken, token).then(function() {
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
	var numIds = req.body.numIds || 1;
	if (!req.body.friendId) {
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
	var contentType = req.body.contentType;

	if (!fileId || !method || !contentType) {
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
	var firstName = req.body.firstName || '';
	var lastName = req.body.lastName || '';

	if (!firstName) {
		sendError(res);
		return;
	}

	req.user.update(User.fields.firstName, firstName, User.fields.lastName, lastName).then(function() {
		sendSuccess(res);
	}, function() {
		sendError(res);
	});
});

/*
 * Request parameters:
 * name - The name of the group.
 */
router.post('/group/create/', function(req, res) {
	var name = req.body.name;

	if (!name) {
		sendError(res);
		return;
	}

	Group.create(name, req.user).then(function(group) {
		sendSuccess(res, {
			'groupId': group.id
		});
	}, function() {
		sendError(res);
	});
});

/*
 * Request parameters:
 * groupId - The id of the group.
 * friendId - The id of the friend to add.
 */
router.post('/group/add/', function(req, res) {
	var groupId = req.body.groupId;
	var friendId = req.body.friendId;

	if (!groupId || !friendId) {
		sendError(res);
		return;
	}

	var group = new Group(groupId);
	group.isMember(req.user).then(function(isMember) {
		if (!isMember) {
			return Promise.reject();
		}

		group.addMember(new User(friendId));
	}).then(function() {
		sendSuccess(res);
	}, function() {
		sendError(res);
	});
});

module.exports = router;