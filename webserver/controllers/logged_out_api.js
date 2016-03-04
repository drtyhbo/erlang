var express = require('express'),
	twilio = require('../utils/twilio'),
	User = require('../models/user').User;

var router = express.Router();

router.post('/register/', function(req, res) {
	var phoneNumber = req.body.phone;
	User.create(phoneNumber).then(function(user) {
		return user.fetch(User.fields.code);
	}).then(function(values) {
		twilio.sendSMS(phoneNumber, values[0], function(err, message) {
			res.send({ 'status': 'ok' });
		});
	}, function(err) {
		res.send({'status': 'error'});
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

	var sharedUser;
	User.login(phoneNumber, code).then(function(user) {
		sharedUser = user;
		return user.updatePreKeys(preKeys[0]);
	}).then(function() {
		return sharedUser.fetch(User.fields.firstName, User.fields.lastName, User.fields.session);
	}).then(function(values){
		res.send({
			'status': 'ok',
			'id': sharedUser.id,
			'firstName': values[0],
			'lastName': values[1],
			'sessionToken': values[2]
		});
	}, function(err) {
		res.send({ 'status': 'error' });
	});
});

module.exports = router;