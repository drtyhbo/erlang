var express = require('express'),
	user = require('../models/user'),
	file = require('../models/file'),
	s3 = require('../utils/s3');

var router = express.Router();

router.use(function(req, res, next) {
	var id = req.body.id;
	var sessionToken = req.body.session;
	user.confirmSession(id, sessionToken, function(err, confirmed) {
		if (confirmed) {
			req.userId = id;
			req.sessionToken = sessionToken;

			next();
		} else {
			res.send({
				'status': 'session'
			});
		}
	});
});

/*
 * Request parameters:
 * userId - Current user id.
 * session - Current user session.
 * phone - The phone numbers to check.
 */
router.post('/friend/check/', function(req, res) {
	var phone = req.body.phone;
	if (!(phone instanceof Array)) {
		phone = [phone];
	}
	user.checkUsersWithPhoneNumbers(phone, function(err, users) {
		var result = {
			'status': err || 'ok'
		};
		if (users) {
			result['friends'] = users;
		}
		res.send(result);
	});
});

/*
 * Request parameters:
 * userId - Current user id.
 * session - Current user session.
 * token - The device token.
 */
router.post('/pns/register/', function(req, res) {
	var token = req.body.token;
	user.setDeviceToken(req.userId, token, function(err) {
		res.send({
			'status': err || 'ok'
		});
	});
});

/*
 * Request parameters:
 * userId - Current user id.
 * session - Current user session.
 * friendId - The friend whom should have access.
 */
router.post('/file/create/', function(req, res) {
	var numIds = req.body.numIds || 1;
	file.create(req.userId, req.body.friendId, numIds, function(err, fileId) {
		var result = {
			'status': err || 'ok'
		};
		if (fileId != undefined) {
			result['fileId'] = fileId
		}

		res.send(result);
	});
});

/*
 * Request parameters:
 * userId - Current user id.
 * session - Current user session.
 * fileId - The id of the file.
 */
router.post('/file/get/', function(req, res) {
	file.hasAccess(req.body.fileId, req.userId, function(err, isMember) {
		var result = {
			'status': err || 'ok'
		};
		if (isMember) {
			result['fileUrl'] = file.generateSignedUrl(req.body.fileId, req.body.method.toUpperCase(), req.body.contentType);
		}

		res.send(result);
	});
});

/*
 * Request parameters:
 * userId - Current user id.
 * session - Current user session.
 */
router.post('/profilepic/', function(req, res) {
	res.send({
		'uploadUrl': s3.generateSignedUrl('PUT', req.userId, 'drtyhbo-chat-users', 'image/jpeg')
	});
});

/*
 * Request parameters:
 * userId - Current user id.
 * session - Current user session.
 * url - The id of the file.
 */
router.post('/url/metadata/', function(req, res) {
	file.hasAccess(req.body.fileId, req.userId, function(err, isMember) {
		var result = {
			'status': err || 'ok'
		};
		if (isMember) {
			result['fileUrl'] = file.generateSignedUrl(req.body.fileId, req.body.method.toUpperCase(), req.body.contentType);
		}

		res.send(result);
	});
});


module.exports = router;