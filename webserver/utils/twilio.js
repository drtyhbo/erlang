var twilio = require('twilio');
var client = new twilio.RestClient('ACf35df294df44300a91862f2583733c3e', '4b3d176ad111aac4a48f589a89471c48');

exports.sendSMS = function(to, body, cb) {
	client.sms.messages.create({
	    to: '+' + to,
	    from: '18312747128',
	    body: body
	}, cb);
}