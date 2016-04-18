var express = require('express'),
	twilio = require('../utils/twilio'),
	User = require('../models/user').User;

var router = express.Router();

router.post('/register/', function(req, res) {
	var phoneNumber = req.body.phone;
	var deviceUuid = req.body.deviceUuid;

	if (!phoneNumber || !deviceUuid) {
		res.send({ 'status': 'error' });
		return;
	}

	User.create(phoneNumber, deviceUuid).then(function(values) {
		var code = values[2];
		twilio.sendSMS(phoneNumber, code, function(err, message) {
			res.send({ 'status': 'ok' });
		});
	}, function(err) {
		console.log(err.message);
		res.send({'status': 'error'});
	});
});

router.post('/confirm/', function(req, res) {
	var phoneNumber = req.body.phone;
	var code = req.body.code;
	var deviceUuid = req.body.deviceUuid;
	var preKeys = req.body.preKeys;

	if (!phoneNumber || !code || !deviceUuid || !preKeys || !preKeys[0]) {
		res.send({ 'status': 'error' });
		return;
	}

	var sharedUser;
	var sharedDevice;
	User.verifyNumber(phoneNumber, deviceUuid, code).then(function(values) {
		sharedUser = values[0];
		sharedDevice = values[1]
		return sharedDevice.updatePreKeys(preKeys[0]);
	}).then(function() {
		return sharedDevice.login();
	}).then(function(sessionToken) {
		res.send({
			'status': 'ok',
			'id': sharedUser._id.toString(),
			'deviceId': sharedDevice._id.toString(),
			'firstName': sharedUser.first || null,
			'lastName': sharedUser.last || null,
			'sessionToken': sessionToken
		});
	}, function(err) {
		res.send({ 'status': 'error' });
	});
});

module.exports = router;
