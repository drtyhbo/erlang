var express = require('express'),
	User = require('../models/user').User,
	File = require('../models/file').File,
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
		res.send({ 'status': 'error' });
		return;
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
	if (!token) {
		res.send({ 'status': 'error' })
		return;
	}

	req.user.update(User.fields.iosPushToken, token).then(function() {
		res.send({ 'status': 'ok' })
	}, function() {
		res.send({ 'status': 'error' });
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
	if (!req.body.friendId) {
		res.send({ 'status': 'error' });
		return;
	}

	File.create(req.user, new User(req.body.friendId), numIds).then(function(files) {
		res.send({
			'status': 'ok',
			'fileIds': files.map(function(file) { return file.id }) 
		});
	}, function() {
		res.send({ 'status': 'error' });
	});
});

/*
 * Request parameters:
 * userId - Current user id.
 * session - Current user session.
 * fileId - The id of the file.
 */
router.post('/file/get/', function(req, res) {
	var fileId = req.body.fileId;
	var method = req.body.method;
	var contentType = req.body.contentType;

	if (!fileId || !method || !contentType) {
		res.send({ 'status': 'error' });
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
		res.send({ 'status': 'error' })
	});
});

/*
 * Request parameters:
 * userId - Current user id.
 * session - Current user session.
 */
router.post('/profilepic/', function(req, res) {
	res.send({
		'status': 'ok',
		'uploadUrl': s3.generateSignedUrl('PUT', req.user.id, 'drtyhbo-chat-users', 'image/jpeg')
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
	var firstName = req.body.firstName || '';
	var lastName = req.body.lastName || '';

	if (!firstName) {
		res.send({ 'status': 'error' });
		return;
	}

	req.user.update(User.fields.firstName, firstName, User.fields.lastName, lastName).then(function() {
		res.send({ 'status': 'ok' })
	}, function() {
		res.send({ 'status': 'error' })
	});
});


module.exports = router;