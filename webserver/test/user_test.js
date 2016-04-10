var assert = require('assert'),
	mongo = require('../models/mongo'),
	request = require('supertest'),
	Device = require('../models/device').Device,
	Promise = require('bluebird').Promise,
	User = require('../models/user').User;

var Constants = {
	phoneNumber: '18315550835',
	deviceUuid: '240b1900895e4b5d907caf0538464838',
	keyOfLastResort: 'abcdefgh'
};

function deleteUser(phoneNumber) {
	return User.find({ phone: phoneNumber }).remove();
}

function deleteDevice(deviceUuid) {
	return Device.find({ deviceUuid: deviceUuid }).remove();
}

describe('User', function() {
	var sharedUser;
	var sharedDevice;
	var sharedCode;

	before(function (done) {
		var promises = [];
		promises.push(deleteUser(Constants.phoneNumber));
		promises.push(deleteDevice(Constants.deviceUuid));
		Promise.all(promises).then(function() {
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
		User.create(Constants.phoneNumber, Constants.deviceUuid).then(function(values) {
			sharedUser = values[0];
			sharedDevice = values[1];
			sharedCode = values[2];

			assert.notEqual(sharedUser, null);
			assert.equal(sharedUser.phone, Constants.phoneNumber);

			assert.notEqual(sharedDevice, null);
			assert.equal(sharedDevice.deviceUuid, Constants.deviceUuid);

			assert.equal(sharedCode >= 100000, true);
			assert.equal(sharedCode <= 999999, true);

			done();
		});
	});

	it('User - verify number no code', function testSlash(done) {
		User.verifyNumber(Constants.phoneNumber, Constants.deviceUuid).then(function(user) {
		}, function(err) {
			done();
		});
	});

	it('User - verify number no phone', function testSlash(done) {
		User.verifyNumber(null, '12345').then(function(user) {
		}, function(err) {
			done();
		});
	});

	it('User - verify number ok', function testSlash(done) {
		User.verifyNumber(Constants.phoneNumber, Constants.deviceUuid, sharedCode).then(function(values) {
			assert.equal(values.length, 2);
			assert.notEqual(values[0], null);
			assert.notEqual(values[1], null);

			done();
		});
	});

	it('User - checkPhoneNumbers', function testSlash(done) {
		var promises = [];

		promises.push(User.create('18315551111', '332af0262d8344d4b0cfda35b7f38d66'));
		promises.push(User.create('18315552222', 'b4d0f58c3bd644fea15d834fbdf6b3d5'));

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

/*	it('User - fetch/update', function testSlash(done) {
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
	});*/
});
