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

module.exports = router;