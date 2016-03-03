var assert = require('assert'),
	request = require('supertest'),
	redis = require('../models/redis').redis,
	Promise = require('bluebird').Promise,
	File = require('../models/file').File,
	User = require('../models/user').User,
	helpers = require('./test_helpers');

const Constants = {
	phoneNumber: '18315550835',
};

describe('file', function() {
	var sharedUser;

	before(function (done) {
		helpers.deleteUser(Constants.phoneNumber).then(function() {
			return User.create(Constants.phoneNumber);
		}).then(function(user) {
			sharedUser = user;
			done();
		});
	});

	it('create - invalid friend', function testSlash(done) {
		File.create(sharedUser, new User(-1), 1).then(function() {
			done();
			// This should not be called.
		}, function() {
			done();
		});
	});

	it('create', function testSlash(done) {
		User.create('18315551111').then(function(friend) {
			return File.create(sharedUser, friend, 1);
		}).then(function(files) {
			assert.equal(files.length, 1);
			done();
		}, function(err) {
			console.log(err);
			done();
		});
	});

	it('create multiple', function testSlash(done) {
		User.create('18315551111').then(function(friend) {
			return File.create(sharedUser, friend, 2);
		}).then(function(files) {
			assert.equal(files.length, 2);
			done();
		}, function(err) {
			console.log(err);
			done();
		});
	});
});