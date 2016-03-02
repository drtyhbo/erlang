var express = require('express'),
	twilio = require('../utils/twilio'),
	user = require('../models/user');

var router = express.Router();

router.post('/register/', function(req, res) {
	var phoneNumber = req.body.phone;
	if (!phoneNumber || phoneNumber.length != 11) {
		res.send({ 'status': 'error' });
		return;
	}

	user.create(phoneNumber, function(err, code) {
		twilio.sendSMS(phoneNumber, code, function(err, message) {
			res.send({ 'status': 'ok' });
		});
	});
});

router.post('/confirm/', function(req, res) {
	var phoneNumber = req.body.phone;
	var code = req.body.code;
	var preKeys = req.body.preKeys;
	
	if (!phoneNumber || !code || !preKeys || !preKeys[0]) {
		res.send({ 'status': 'error' });
		return;
	}

	user.login(phoneNumber, code, preKeys[0], function(err, id, sessionToken, firstName, lastName) {
		if (err) {
			res.send({ 'status': 'error' });
			return;
		}

		res.send({
			'status': 'ok',
			'id': id,
			'sessionToken': sessionToken,
			'firstName': firstName,
			'lastName': lastName
		});
	});
});

module.exports = router;