var express = require('express'),
	twilio = require('../utils/twilio'),
	user = require('../models/user');

var router = express.Router();

router.post('/register/', function(req, res) {
	var phoneNumber = req.body.phone;
	user.create(phoneNumber, function(err, code) {
		twilio.sendSMS(phoneNumber, "Your code is " + code, function(err, message) {
			res.send({
				'status': 'ok'
			});
		});
	});
});

router.post('/confirm/', function(req, res) {
	var phoneNumber = req.body.phone;
	var code = req.body.code;
	var key = req.body.key;
	user.login(phoneNumber, code, key, function(err, id, sessionToken) {
		if (err) {
			res.send({
				'status': err
			});
		} else {
			res.send({
				'status': 'ok',
				'id': id,
				'sessionToken': sessionToken
			});
		}
	});
});

module.exports = router;