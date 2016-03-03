var assert = require('assert'),
	request = require('supertest'),
	redis = require('../models/redis').redis,
	Promise = require('bluebird').Promise,
	User = require('../models/user').User,
	helpers = require('./test_helpers');

var Constants = {
	phoneNumber: '18315550835',
	phoneNumberKey: 'p:{18315550835}'
};

describe('User', function() {
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
	
	it('User - missing phone number', function testSlash(done) {
		User.create().then(function() {
		}, function() {
			done();
		});
	});
	
	it('User - short phone number', function testSlash(done) {
		User.create('1234').then(function() {
		}, function() {
			done();
		});
	});

	it('User - create ok', function testSlash(done) {
		User.create(Constants.phoneNumber).then(function(user) {
			assert.notEqual(user, null);
			return user.fetch(User.fields.code);
		}).then(function(values) {
			assert.equal(values[0] >= 100000, true);
			assert.equal(values[0] <= 999999, true);
			done();
		});
	});

	it('User - login no code', function testSlash(done) {
		User.login(Constants.phoneNumber, '12345').then(function(user) {
		}, function(err) {
			done();
		});
	});

	it('User - login no phone', function testSlash(done) {
		User.login('12345', '12345').then(function(user) {
		}, function(err) {
			done();
		});
	});

	it('User - login ok', function testSlash(done) {
		redis.getAsync(Constants.phoneNumberKey).then(function(id) {
			var user = new User(id);
			return user.fetch(User.fields.code);
		}).then(function(values) {
			return User.login(Constants.phoneNumber, values[0]);
		}).then(function(user) {
			assert.notEqual(user, null);
			done();
		});
	});
});