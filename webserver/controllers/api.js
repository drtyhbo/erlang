var express = require('express'),
	twilio = require('../utils/twilio'),
	user = require('../models/user');

var router = express.Router();

router.post('/register/', function(req, res) {
	var phoneNumber = req.body.phone;
	user.create(phoneNumber, function(err, code) {
		twilio.sendSMS(phoneNumber, "Your code is " + code, function(err, message) {
			res.send(err + ' ' + message);
		});
	});
});

module.exports = router;