var assert = require('assert'),
	request = require('supertest'),
	Device = require('../models/device').Device,
	User = require('../models/user').User,
	Promise = require('bluebird').Promise;

const Constants = {
	phoneNumber: '18315550835',
	deviceUuid: '240b1900895e4b5d907caf0538464838'
};

function deleteUser(phoneNumber) {
	return User.find({ phone: phoneNumber }).remove();
}

function deleteDevice(deviceUuid) {
	return Device.find({ deviceUuid: deviceUuid }).remove();
}

function getUserDeviceAndCode(phoneNumber, deviceUuid) {
	var sharedUser;
	return User.findUser(phoneNumber).then(function(user) {
		sharedUser = user;
		return Device.findDevice(deviceUuid, user._id);
	}).then(function(device) {
		return Promise.resolve([sharedUser, device, device.code]);
	});
}

describe('logged out', function() {
	var server;

	before(function (done) {
		server = require('../app');

		var promises = [];

		promises.push(deleteUser(Constants.phoneNumber));
		promises.push(deleteDevice(Constants.deviceUuid));

		Promise.all(promises).then(function() {
			done();
		});
	});

	it('/api/register/ - missing phone number', function testSlash(done) {
		request(server)
			.post('/api/register/')
			.send({
				deviceUuid: Constants.deviceUuid
			})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/register/ - short phone number', function testSlash(done) {
		request(server)
			.post('/api/register/')
			.send({
				phone: '12345',
				deviceUuid: Constants.deviceUuid
			})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/register/ - missing device', function testSlash(done) {
		request(server)
			.post('/api/register/')
			.send({
				phone: Constants.phoneNumber
			})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/register/', function testSlash(done) {
		request(server)
			.post('/api/register/')
			.send({
				phone: Constants.phoneNumber,
				deviceUuid: Constants.deviceUuid
			})
			.expect(200, {
				status: 'ok'
			})
			.end(function(err, res) {
				getUserDeviceAndCode(Constants.phoneNumber, Constants.deviceUuid).then(function(values) {
					assert.notEqual(values[0], null);
					assert.notEqual(values[1], null);
					assert.notEqual(values[2], null);

					done();
				});
			});
	});

	it('/api/confirm/ - no phone number', function testSlash(done) {
		request(server)
			.post('/api/confirm/')
			.send({
				code: '123456',
				preKeys: [[]],
				deviceUuid: Constants.deviceUuid
			})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/confirm/ - no code', function testSlash(done) {
		request(server)
			.post('/api/confirm/')
			.send({
				phone: Constants.phoneNumber,
				preKeys: [[]],
				deviceUuid: Constants.deviceUuid
			})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/confirm/ - no pre keys', function testSlash(done) {
		request(server)
			.post('/api/confirm/')
			.send({
				phone: Constants.phoneNumber,
				code: '123456',
				deviceUuid: Constants.deviceUuid
			})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/confirm/ - no device uuid', function testSlash(done) {
		request(server)
			.post('/api/confirm/')
			.send({
				phone: Constants.phoneNumber,
				code: '123456',
				preKeys: [[]]
			})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/confirm/ - success', function testSlash(done) {
		getUserDeviceAndCode(Constants.phoneNumber, Constants.deviceUuid).then(function(values) {
			var id = values[0].id;
			var code = values[2];
			request(server)
				.post('/api/confirm/')
				.send({
					phone: Constants.phoneNumber,
					code: code,
					preKeys: [{
						i: [0],
						pk: ['abcd']
					}],
					deviceUuid: Constants.deviceUuid
				})
				.expect(function(res) {
					if (!res.body.sessionToken || res.body.sessionToken.length != 64) {
						return "invalid session token";
					}
					if (!res.body.id || res.body.id.length != 24) {
						return "invalid user id";
					}
					if (!res.body.deviceId || res.body.deviceId.length != 24) {
						return "invalid device id";
					}
					res.body.sessionToken = "abcd";
					res.body.deviceId = 100;
				})
				.expect(200, {
					status: 'ok',
					firstName: null,
					lastName: null,
					id: id,
					sessionToken: "abcd",
					deviceId: 100
				}, done);
		});
	});

	it('/api/confirm/ - invalid code', function testSlash(done) {
		getUserDeviceAndCode(Constants.phoneNumber, Constants.deviceUuid).then(function(values) {
			var id = values[0].id;
			var code = values[2];
			request(server)
				.post('/api/confirm/')
				.send({
					phone: Constants.phoneNumber,
					code: code + '1',
					preKeys: [{
						i: [0],
						pk: ['abcd']
					}],
					deviceUuid: Constants.deviceUuid
				})
				.expect(200, {
					status: 'error'
				}, done);
		});
	});

	it('/api/confirm/ - check redis objects', function testSlash(done) {
		getUserDeviceAndCode(Constants.phoneNumber, Constants.deviceUuid).then(function(values) {
			var id = values[0].id;
			var device = values[1];
			var code = values[2];
			request(server)
				.post('/api/confirm/')
				.send({
					phone: Constants.phoneNumber,
					code: code,
					preKeys: [{
						i: [0, 1],
						pk: ['abcd', 'efgh']
					}],
					deviceUuid: Constants.deviceUuid
				})
				.end(function(err, res) {
					// Check keys
					done();
				});
		});
	});
});
