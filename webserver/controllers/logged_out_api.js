var express = require('express'),
	twilio = require('../utils/twilio'),
	user = require('../models/user');

var router = express.Router();

router.post('/register/', function(req, res) {
	var phoneNumber = req.body.phone;
	if (!phoneNumber || phoneNumber.length != 11) {
		res.send({
			'status': 'error'
		});
	} else {
		user.create(phoneNumber, function(err, code) {
			twilio.sendSMS(phoneNumber, code, function(err, message) {
				res.send({
					'status': 'ok'
				});
			});
		});
	}
});

router.post('/confirm/', function(req, res) {
	var phoneNumber = req.body.phone;
	var code = req.body.code;
	var preKeys = req.body.preKeys[0];
	user.login(phoneNumber, code, preKeys, function(err, id, sessionToken, firstName, lastName) {
		if (err) {
			res.send({
				'status': err
			});
		} else {
			res.send({
				'status': 'ok',
				'id': id,
				'sessionToken': sessionToken,
				'firstName': firstName,
				'lastName': lastName
			});
		}
	});
});

module.exports = router;