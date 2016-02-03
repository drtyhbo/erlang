var express = require('express'),
	user = require('../models/user');

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
 * phone - The phone number of the users to check.
 */
router.post('/friend/add/', function(req, res) {
	var phone = req.body.phone;
	user.addFriendFromPhoneNumber(req.userId, phone, function(err) {
		res.send({
			'status': err || 'ok'
		});
	});
});

/*
 * Request parameters:
 * userId - Current user id.
 * session - Current user session.
 * friendId - The id of the friend.
 */
router.post('/friend/accept/', function(req, res) {
	var friendUserId = req.body.friendId;
	user.addFriendFromId(req.userId, friendUserId, function(err) {
		res.send({
			'status': err || 'ok'
		});
	});
});

/*
 * Request parameters:
 * userId - Current user id.
 * session - Current user session.
 * friendId - The id of the friend.
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

 */
router.post('/friend/all/', function(req, res) {
	user.getFriendsForUserWithId(req.userId, function(err, friends) {
		var result = {
			'status': err || 'ok'
		};
		if (friends) {
			result['friends'] = friends;
		}
		res.send(result);
	});
});

module.exports = router;