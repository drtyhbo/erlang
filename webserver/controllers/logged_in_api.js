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
 * phone - The phone number of the friend.
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

module.exports = router;