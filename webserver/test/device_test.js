var assert = require('assert'),
	mongo = require('../models/mongo'),
	request = require('supertest'),
	Promise = require('bluebird').Promise,
	Device = require('../models/device').Device,
	User = require('../models/user').User;

var Constants = {
	phoneNumber: '18315550835',
	deviceUuid: '729908c5a45746af90a88b53a738c218',
	keyOfLastResort: 'abcdefgh'
};

function deleteUser(phoneNumber) {
	return User.find({ phone: phoneNumber }).remove();
}

function deleteDevice(deviceUuid) {
	return Device.find({ deviceUuid: deviceUuid }).remove();
}

describe('Device', function() {
	var sharedUser;
	var sharedDevice;

	before(function (done) {
		var promises = [];

		promises.push(deleteUser(Constants.phoneNumber).then(function() {
			return new User({ phone: Constants.phoneNumber }).save();
		}).then(function(user) {
			sharedUser = user;
		}));
		promises.push(deleteDevice(Constants.deviceId));

		Promise.all(promises).then(function() {
			done();
		});
	});

	it('Device - missing deviceId', function testSlash(done) {
		Device.create(null, '1234').then(function() {
		}, function() {
			done();
		});
	});

	it('Device - missing userId', function testSlash(done) {
		Device.create(Constants.deviceUuid, null).then(function() {
		}, function() {
			done();
		});
	});

	it('Device - create ok', function testSlash(done) {
		Device.create(Constants.deviceUuid, sharedUser._id).then(function(device) {
			assert.equal(device.deviceUuid, Constants.deviceUuid);
			assert.equal(device.userId, sharedUser._id);

			sharedDevice = device;

			done();
		});
	});

	it('Device - create duplicate ok', function testSlash(done) {
		Device.create(Constants.deviceUuid, sharedUser._id).then(function(device) {
			assert.equal(sharedDevice._id.toString(), device._id.toString());

			done();
		});
	});

	it('Device - generate code', function testSlash(done) {
		sharedDevice.generateCode().then(function(code) {
			assert.equal(code >= 100000, true);
			assert.equal(code <= 999999, true);

			done();
		});
	});

	it('Device - generate code same', function testSlash(done) {
		var firstCode;
		sharedDevice.generateCode().then(function(code) {
			assert.notEqual(code, null);

			firstCode = code;

			return sharedDevice.generateCode();
		}).then(function(code) {
			assert.equal(firstCode, code);

			done();
		});
	});

	it('Device - find device for user no user id', function testSlash(done) {
		Device.findDeviceForUser(Constants.deviceUuid, null).then(function(device) {
		}, function() {
			done();
		});
	});

	it('Device - find device for user no device uuid', function testSlash(done) {
		Device.findDeviceForUser(null, sharedUser._id).then(function(device) {
		}, function() {
			done();
		});
	});

	it('Device - find device for user ok', function testSlash(done) {
		Device.findDeviceForUser(Constants.deviceUuid, sharedUser._id).then(function(device) {
			assert.equal(device.deviceUuid, Constants.deviceUuid);
			assert.equal(device.userId.toString(), sharedUser._id.toString());
			
			done();
		});
	});

	/*it('User - verify number no code', function testSlash(done) {
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
	});*/
});
