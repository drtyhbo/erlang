var assert = require('assert'),
	request = require('supertest'),
	redis = require('../models/redis').redis,
	Device = require('../models/device').Device,
	User = require('../models/user').User,
	Promise = require('bluebird').Promise,
	helpers = require('./test_helpers');

const Constants = {
	phoneNumber: '18315550835',
	phoneNumberKey: 'p:{18315550835}',
	deviceUuid: '729908c5a45746af90a88b53a738c218'
};

function getUserDeviceAndCode(phoneNumber, deviceUuid) {
	var sharedUser;
	var sharedDevice;
	return redis.getAsync(Constants.phoneNumberKey).then(function(userId) {
		sharedUser = new User(userId);
		return sharedUser.findDevice(deviceUuid);
	}).then(function(device) {
		sharedDevice = device;
		return device.fetch(Device.fields.code);
	}).then(function(values) {
		return Promise.resolve([sharedUser, sharedDevice, values[0]]);
	});
}

describe('logged out', function() {
	var server;

	before(function (done) {
		server = require('../app');
		helpers.deleteUser(Constants.phoneNumber).then(function() {
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
				redis.getAsync(Constants.phoneNumberKey).then(function(userId) {
					assert.notEqual(userId, null);
					return redis.hget('u:{' + userId + '}', 'd:' + Constants.deviceUuid);
				}).then(function(deviceId) {
					return redis.hgetAsync('d:{' + deviceId + '}', 'code');
				}).then(function(code) {
					assert.notEqual(code, null);
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
					if (!res.body.deviceId) {
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
					redis.smembersAsync('pki:{' + device.id + '}').then(function(values) {
						assert.equal(values[0], '0');
						assert.equal(values[1], '1');
						return redis.hgetallAsync('pk:{' + device.id + '}');
					}).then(function(values) {
						assert.equal(values['0'], 'abcd');
						assert.equal(values['1'], 'efgh');
						return redis.hget('u:{' + id + '}', 'd:' + Constants.deviceUuid);
					}).then(function(value) {
						assert.equal(value, res.body.deviceId);
						return redis.hget('d:{' + res.body.deviceId + '}', 'session');
					}).then(function(value) {
						assert.equal(value, res.body.sessionToken);
						done();
					});
				});
		});
	});
});