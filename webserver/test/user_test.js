var assert = require('assert'),
	request = require('supertest'),
	redis = require('../models/redis').redis,
	Promise = require('bluebird').Promise,
	User = require('../models/user').User,
	helpers = require('./test_helpers');

var Constants = {
	phoneNumber: '18315550835',
	keyOfLastResort: 'abcdefgh'
};

describe('User', function() {
	var sharedUser;

	before(function (done) {
		helpers.deleteUser(Constants.phoneNumber).then(function() {
			done();
		});
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
			sharedUser = user;

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
		sharedUser.fetch(User.fields.code).then(function(values) {
			return User.login(Constants.phoneNumber, values[0]);
		}).then(function(user) {
			assert.notEqual(user, null);
			done();
		});
	});

	it('User - checkPreKeys none', function testSlash(done) {
		sharedUser.fetchPreKey().then(function(key) {
			// This should not be called.
		}, function() {
			done();
		});
	});

	it('User - update pre keys', function testSlash(done) {
		function updatePreKeys(keys) {
			assert.equal(keys.length, 3);
			return sharedUser.updatePreKeys({
				i: [0, 1, 0xFFFF],
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

		updatePreKeys(['abcd', 'efgh', Constants.keyOfLastResort]).then(function(ok) {
			assert.equal(ok, true);
			return validatePreKeys(['abcd', 'efgh']);
		}).then(function() {
			return updatePreKeys(['ijkl', 'mnop', Constants.keyOfLastResort]);
		}).then(function() {
			return validatePreKeys(['ijkl', 'mnop']);
		}).then(function() {
			done();
		});
	});

	it('User - fetch/update', function testSlash(done) {
		sharedUser.update(
				User.fields.session, 'abcd',
				User.fields.firstName, 'Rob',
				User.fields.lastName, 'Lowe',
				User.fields.iosPushToken, 'jklm').then(function() {
			return sharedUser.fetch(User.fields.session, User.fields.firstName, User.fields.lastName, User.fields.iosPushToken);
		}).then(function(values) {
			assert.notEqual(values[0], 'abcd');
			assert.equal(values[1], 'Rob');
			assert.equal(values[2], 'Lowe');
			assert.equal(values[3], 'jklm');
			done();
		});
	});

	it('User - fetch/update array', function testSlash(done) {
		sharedUser.update(
				[User.fields.session, 'abcd2',
				User.fields.firstName, 'Rob2',
				User.fields.lastName, 'Lowe2',
				User.fields.iosPushToken, 'jklm2']).then(function() {
			return sharedUser.fetch(User.fields.session, User.fields.firstName, User.fields.lastName, User.fields.iosPushToken);
		}).then(function(values) {
			assert.notEqual(values[0], 'abcd2');
			assert.equal(values[1], 'Rob2');
			assert.equal(values[2], 'Lowe2');
			assert.equal(values[3], 'jklm2');
			done();
		});
	});

	it('User - confirmSession', function testSlash(done) {
		sharedUser.fetch(User.fields.session).then(function(values) {
			return sharedUser.confirmSession(values[0]);
		}).then(function(user) {
			assert.notEqual(user, null);
			done();
		});
	});

	it('User - confirmSession null session', function testSlash(done) {
		sharedUser.confirmSession().then(function() {
			// Should never get here.
		}, function(err) {
			done();
		});
	});

	it('User - checkPhoneNumbers', function testSlash(done) {
		var promises = [];
		promises.push(User.create('18315551111'));
		promises.push(User.create('18315552222'));
		Promise.all(promises).then(function() {
			return User.checkPhoneNumbers(['18315551111', '18315553333', '18315552222']);
		}).then(function(phoneNumbers) {
			assert.notEqual(phoneNumbers[0].id, null);
			assert.equal(phoneNumbers[0].phone, '18315551111');
			assert.equal(phoneNumbers[1], null);
			assert.notEqual(phoneNumbers[2].id, null);
			assert.equal(phoneNumbers[2].phone, '18315552222');
			done();
		});
	});

	it('User - checkPreKeys ok', function testSlash(done) {
		sharedUser.fetchPreKey().then(function(key) {
			assert.notEqual(key.index, null);
			assert.notEqual(key.index, 0xFFFF);
			assert.notEqual(key.key, null);
			return sharedUser.fetchPreKey();
		}).then(function(key) {
			assert.notEqual(key.index, null);
			assert.notEqual(key.index, 0xFFFF);
			assert.notEqual(key.key, null);
			return sharedUser.fetchPreKey();
		}).then(function(key) {
			assert.equal(key.index, 0xFFFF);
			assert.equal(key.key, Constants.keyOfLastResort);
			return sharedUser.fetchPreKey();
		}).then(function(key) {
			assert.equal(key.index, 0xFFFF);
			assert.equal(key.key, Constants.keyOfLastResort);
			done();
		});
	});
});