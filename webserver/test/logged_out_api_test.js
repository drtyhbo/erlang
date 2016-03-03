var assert = require('assert'),
	request = require('supertest'),
	redis = require('../models/redis').redis,
	Promise = require('bluebird').Promise,
	helpers = require('./test_helpers');

const Constants = {
	phoneNumber: '18315550835',
	phoneNumberKey: 'p:{18315550835}'
};

function getIdAndCodeForPhoneNumber(phoneNumber) {
	var sharedId;
	return redis.getAsync(Constants.phoneNumberKey).then(function(value) {
		if (value) {
			sharedId = value
			return redis.hmgetAsync('u:{' + value + '}', 'code');
		} else {
			return Promise.reject()
		}
	}).then(function(code) {
		return Promise.resolve([sharedId, code[0]]);
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

	afterEach(function () {
		server.close();
	});
	
	it('/api/register/ - missing phone number', function testSlash(done) {
		request(server)
			.post('/api/register/')
			.expect(200, {
				status: 'error'
			}, done);
	});
	
	it('/api/register/ - short phone number', function testSlash(done) {
		request(server)
			.post('/api/register/')
			.send({
				phone: '12345'
			})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/register/ - phone number', function testSlash(done) {
		request(server)
			.post('/api/register/')
			.send({
				phone: Constants.phoneNumber
			})
			.expect(200, {
				status: 'ok'
			})
			.end(function(err, res) {
				redis.getAsync(Constants.phoneNumberKey).then(function(userId) {
					assert.notEqual(userId, null);
					return redis.hmgetAsync('u:{' + userId + '}', 'code');
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
				preKeys: [[]]
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
				preKeys: [[]]
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
				code: '123456'
			})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/confirm/ - success', function testSlash(done) {
		getIdAndCodeForPhoneNumber(Constants.phoneNumber).then(function(values) {
			var id = values[0];
			var code = values[1];
			request(server)
				.post('/api/confirm/')
				.send({
					phone: Constants.phoneNumber,
					code: code,
					preKeys: [{
						i: [0],
						pk: ['abcd']
					}]
				})
				.expect(function(res) {
					if (!res.body.sessionToken || res.body.sessionToken.length != 64) {
						return "invalid session token";
					}
					res.body.sessionToken = "abcd";
				})
				.expect(200, {
					status: 'ok',
					firstName: null,
					lastName: null,
					id: id,
					sessionToken: "abcd"
				}, done);
		});
	});

	it('/api/confirm/ - invalid code', function testSlash(done) {
		getIdAndCodeForPhoneNumber(Constants.phoneNumber).then(function(values) {
			var id = values[0];
			var code = values[1];
			request(server)
				.post('/api/confirm/')
				.send({
					phone: Constants.phoneNumber,
					code: code + '1',
					preKeys: [{
						i: [0],
						pk: ['abcd']
					}]
				})
				.expect(200, {
					status: 'error'
				}, done);
		});
	});

	it('/api/confirm/ - pre keys', function testSlash(done) {
		getIdAndCodeForPhoneNumber(Constants.phoneNumber).then(function(values) {
			var id = values[0];
			var code = values[1];
			request(server)
				.post('/api/confirm/')
				.send({
					phone: Constants.phoneNumber,
					code: code,
					preKeys: [{
						i: [0, 1],
						pk: ['abcd', 'efgh']
					}]
				})
				.end(function(err, res) {
					redis.smembersAsync('pki:{' + id + '}').then(function(values) {
						assert.equal(values[0], '0');
						assert.equal(values[1], '1');
						return redis.hgetallAsync('pk:{' + id + '}');
					}).then(function(values) {
						assert.equal(values['0'], 'abcd');
						assert.equal(values['1'], 'efgh');
						done();
					});
				});
		});
	});
});