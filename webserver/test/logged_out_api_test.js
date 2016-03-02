var assert = require('assert'),
	request = require('supertest'),
	redis = require('../models/redis').redis,
	Promise = require('bluebird').Promise;

var constants = {
	phoneNumber: '18315550835',
	phoneNumberKey: 'p:{18315550835}'
};

describe('registration', function() {
	var server;

	beforeEach(function (done) {
		server = require('../app');
		redis.getAsync(constants.phoneNumberKey).then(function(value) {
			if (value) {
				return redis
					.multi()
					.del(constants.phoneNumberKey)
					.del('u:{' + value + '}')
					.exec();
			} else {
				return Promise.resolve();
			}
		}).then(function() {
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
				phone: constants.phoneNumber
			})
			.expect(200, {
				status: 'ok'
			})
			.end(function(err, res) {
				redis.getAsync(constants.phoneNumberKey).then(function(userId) {
					assert.notEqual(userId, null);
					return redis.hmgetAsync('u:{' + userId + '}', 'code');
				}).then(function(code) {
					assert.notEqual(code, null);
					done();
				});
			});
	});
});