var assert = require('assert'),
	request = require('supertest'),
	redis = require('../models/redis').redis,
	Promise = require('bluebird').Promise,
	User = require('../models/user').User,
	helpers = require('./test_helpers');

var phoneNumber = '18315550835';

describe('User', function() {
	var server;

	before(function (done) {
		server = require('../app');
		helpers.deleteUser(phoneNumber).then(function() {
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

	it('User - ok', function testSlash(done) {
		User.create(phoneNumber).then(function(user) {
			assert.notEqual(user, null);
			return user.getCode();
		}).then(function(code) {
			assert.equal(code >= 100000, true);
			assert.equal(code <= 999999, true);
			done();
		});
	});
});