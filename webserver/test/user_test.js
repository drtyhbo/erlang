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

	it('User - update pre keys', function testSlash(done) {
		var sharedUser;

		function updatePreKeys(keys) {
			assert.equal(keys.length, 2);
			return sharedUser.updatePreKeys({
				i: [0, 1],
				pk: keys
			});
		}

		function validatePreKeys(keys) {
			assert.equal(keys.length, 2);
			return redis.smembersAsync('pki:{' + sharedUser.id + '}').then(function(values) {
				assert.equal(values[0], '0');
				assert.equal(values[1], '1');
				return redis.hgetallAsync('pk:{' + sharedUser.id + '}');
			}).then(function(values) {
				assert.equal(values['0'], keys[0]);
				assert.equal(values['1'], keys[1]);
				return Promise.resolve()
			});
		}

		redis.getAsync(Constants.phoneNumberKey).then(function(id) {
			sharedUser = new User(id);
			return updatePreKeys(['abcd', 'efgh']);
		}).then(function(ok) {
			assert.equal(ok, true);
			return validatePreKeys(['abcd', 'efgh']);
		}).then(function() {
			return updatePreKeys(['ijkl', 'mnop']);
		}).then(function() {
			return validatePreKeys(['ijkl', 'mnop']);
		}).then(function() {
			done();
		});
	});

	it('User - fetch/update', function testSlash(done) {
		var sharedUser;
		redis.getAsync(Constants.phoneNumberKey).then(function(id) {
			sharedUser = new User(id);
			return sharedUser.update(User.fields.session, 'abcd', User.fields.firstName, 'Rob', User.fields.lastName, 'Lowe', User.fields.iosPushToken, 'jklm');
		}).then(function() {
			return sharedUser.fetch(User.fields.session, User.fields.firstName, User.fields.lastName, User.fields.iosPushToken);
		}).then(function(values) {
			assert.notEqual(values[0], 'abcd');
			assert.equal(values[1], 'Rob');
			assert.equal(values[2], 'Lowe');
			assert.equal(values[3], 'jklm');
			done();
		});
	});
});