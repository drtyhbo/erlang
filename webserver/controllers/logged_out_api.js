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
	var sharedSessionToken;
	User.verifyNumber(phoneNumber, deviceUuid, code).then(function(values) {
		sharedUser = values[0];
		sharedDevice = values[1]
		return sharedDevice.updatePreKeys(preKeys[0]);
	}).then(function() {
		return sharedDevice.login();
	}).then(function(sessionToken) {
		sharedSessionToken = sessionToken;
		return sharedUser.fetch(User.fields.firstName, User.fields.lastName);
	}).then(function(values){
		res.send({
			'status': 'ok',
			'id': sharedUser.id,
			'deviceId': sharedDevice.id,
			'firstName': values[0],
			'lastName': values[1],
			'sessionToken': sharedSessionToken
		});
	}, function(err) {
		res.send({ 'status': 'error' });
	});
});

module.exports = router;