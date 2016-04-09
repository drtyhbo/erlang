var assert = require('assert'),
	mongo = require('../models/mongo'),
	request = require('supertest'),
	Device = require('../models/device').Device,
	File = require('../models/file').File,
	Promise = require('bluebird').Promise,
	User = require('../models/user').User;

const Constants = {
	phoneNumber: '18315550835',
	friendNumber: '18315551111',
	deviceUuid: '240b1900-895e-4b5d-907c-af0538464838',
	friendDeviceUuid: '240b1900-895e-4b5d-907c-af0538464839'
};

function createFriend() {
	return User.create(Constants.friendNumber, Constants.friendDeviceUuid).then(function(values) {
		return Promise.resolve(values[0]);
	});
}

function deleteUser(phoneNumber) {
	return User.find({ phone: phoneNumber }).remove();
}

function deleteDevice(deviceUuid) {
	return Device.find({ deviceUuid: deviceUuid }).remove();
}

describe('file', function() {
	var sharedUser;
	var sharedFriend;

	before(function (done) {
		var promises = [];

		promises.push(deleteUser(Constants.phoneNumber));
		promises.push(deleteDevice(Constants.deviceUuid));
		promises.push(deleteUser(Constants.friendNumber));
		promises.push(deleteDevice(Constants.friendDeviceUuid));

		Promise.all(promises).then(function() {
			return User.create(Constants.phoneNumber, Constants.deviceUuid);
		}).then(function(user) {
			sharedUser = user[0];
			return User.create(Constants.friendNumber, Constants.friendDeviceUuid)
		}).then(function(friend) {
			sharedFriend = friend[0];
			done();
		});
	});

	it('create - one user', function testSlash(done) {
		File.create([sharedUser], 1).then(function(files) {
			assert.notEqual(files, null);
			assert.equal(files.length, 1);
			assert.equal(files[0].userIds.length, 1);
			assert.equal(files[0].hasAccess(sharedUser), true);
			assert.equal(files[0].hasAccess(sharedFriend), false);
			done();
		});
	});

	it('create', function testSlash(done) {
		File.create([sharedUser, sharedFriend], 1).then(function(files) {
			assert.notEqual(files, null);
			assert.equal(files.length, 1);
			assert.equal(files[0].userIds.length, 2);
			assert.equal(files[0].hasAccess(sharedUser), true);
			assert.equal(files[0].hasAccess(sharedFriend), true);
			done();
		});
	});

	it('create - multiple', function testSlash(done) {
		File.create([sharedUser, sharedFriend], 2).then(function(files) {
			assert.notEqual(files, null);

			assert.equal(files.length, 2);
			for (var i = 0; i < files.count; i++) {
				assert.equal(files[i].userIds.length, 2);
				assert.equal(files[i].hasAccess(sharedUser), true);
				assert.equal(files[i].hasAccess(sharedFriend), true);
			}

			done();
		});
	});
});
