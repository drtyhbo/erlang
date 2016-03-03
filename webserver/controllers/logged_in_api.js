var express = require('express'),
	user = require('../models/user'),
	User = user.User,
	file = require('../models/file'),
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
		return res.send({ 'status': 'error' });
	});
});

/*
 * Request parameters:
 * userId - Id of the user for whom we need a prekey.
 */
router.post('/friend/prekey/', function(req, res) {
	var userId = req.body.userId;
	if (!userId) {
		res.send({ 'status': 'error' })
		return
	}

	new User(userId).fetchPreKey().then(function(key) {
		res.send({
			'status': 'ok',
			'keyIndex': key.index,
			'publicKey': key.key
		});
	}, function() {
		return res.send({ 'status': 'error' });
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
 * firstName - The user's first name.
 * lastName - The user's last name.
 */
router.post('/info/update/', function(req, res) {
	var firstName = req.body.firstName;
	var lastName = req.body.lastName;
	user.updateInfo(req.userId, firstName, lastName, function(err) {
		res.send({
			'status': err || 'ok'
		});
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