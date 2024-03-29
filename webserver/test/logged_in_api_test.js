var assert = require('assert'),
	request = require('supertest'),
	Promise = require('bluebird').Promise,
	Device = require('../models/device').Device,
	User = require('../models/user').User,
	File = require('../models/file').File;

const Constants = {
	phoneNumber: '18315550835',
	friendNumber: '18315551111',
	deviceUuid: '240b1900895e4b5d907caf0538464838',
	friendDeviceUuid: '240b1900895e4b5d907caf0538464839'
};

function deleteUser(phoneNumber) {
	return User.find({ phone: phoneNumber }).remove();
}

function deleteDevice(deviceUuid) {
	return Device.find({ deviceUuid: deviceUuid }).remove();
}

describe('logged in', function() {
	var server;
	var sharedUser;
	var sharedFriend;
	var sharedDevice;
	var sessionToken;

	before(function (done) {
		server = require('../app');

		var promises = [];

		promises.push(deleteUser(Constants.phoneNumber));
		promises.push(deleteDevice(Constants.deviceUuid));
		promises.push(deleteUser(Constants.friendNumber));
		promises.push(deleteDevice(Constants.friendDeviceUuid));

		Promise.all(promises).then(function() {
			return User.create(Constants.phoneNumber, Constants.deviceUuid);
		}).then(function(values) {
			sharedUser = values[0];
			sharedDevice = values[1];
			code = values[2];
			return User.verifyNumber(Constants.phoneNumber, Constants.deviceUuid, code);
		}).then(function() {
			return sharedDevice.login();
		}).then(function(value) {
			sessionToken = value;
			return User.create(Constants.friendNumber, Constants.friendDeviceUuid)
		}).then(function(values) {
			sharedFriend = values[0];
			done();
		});
	});

	function makeRequest(url, params) {
		params = params || {};
		params['id'] = sharedDevice._id;
		params['session'] = sessionToken;

		return request(server)
			.post(url)
			.send(params);
	}

	it('/api/user/friend/check/', function testSlash(done) {
		var promises = [];
		promises.push(User.create('18315551111', '240b1900895e4b5d907caf0538464837'));
		promises.push(User.create('18315552222', 'f9c9b5fb9ffd4aa087609d8ad18391dd'));
		Promise.all(promises).then(function() {
			makeRequest('/api/user/friend/check/', {
					'phone': [
						'18315551111',
						'18315552222',
						'18315553333'
					]})
				.expect(function(res) {
					if (res.body.friends.length != 3) {
						return "friends field invalid";
					}
					res.body.friends[0].id = '0'
					res.body.friends[1].id = '1'
				})
				.expect(200, {
					status: 'ok',
					friends: [{
						id: '0',
						phone: '18315551111',
					}, {
						id: '1',
						phone: '18315552222',
					},
					null]
				}, done);
		});
	});

	it('/api/user/device/prekey/ - no userId', function testSlash(done) {
		makeRequest('/api/user/device/prekey/')
			.expect(200, {
				status: 'error',
			}, done);
	});

	it('/api/user/device/prekey/ - no keys', function testSlash(done) {
		makeRequest('/api/user/device/prekey/', {
				userId: sharedUser.id})
			.expect(200, {
				status: 'error',
			}, done);
	});

	it('/api/user/device/prekey/', function testSlash(done) {
		return sharedDevice.updatePreKeys({
				i: [0, 0xFFFF],
				pk: ['abcd', 'ijkl']})
			.then(function() {
				makeRequest('/api/user/device/prekey/', {
						deviceId: sharedDevice.id})
					.expect(200, {
						status: 'ok',
						keyIndex: 0,
						publicKey: 'abcd'
					}, done);
			});
	});

	it('/api/user/pns/register/ - no token', function testSlash(done) {
		makeRequest('/api/user/pns/register/')
			.expect(200, {
				status: 'error'
			}, done);
	});


	it('/api/user/pns/register/', function testSlash(done) {
		makeRequest('/api/user/pns/register/', {
				token: 'pnsToken'})
			.expect(200, {
				status: 'error'
			})
			.end(function(err, res) {
				Device.findDeviceById(sharedDevice._id).then(function(device) {
					assert.equal(device.iosPushToken, 'pnsToken');
					done();
				});
			});
	});

	it('/api/user/file/create/ - no friendId', function testSlash(done) {
		makeRequest('/api/user/file/create/', {
				numFiles: 1})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/user/file/create/ - invalid friendId', function testSlash(done) {
		makeRequest('/api/user/file/create/', {
				numFiles: 1,
				friendIds: [-1]})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/user/file/create/', function testSlash(done) {
		makeRequest('/api/user/file/create/', {
				numIds: 1,
				friendIds: [sharedFriend._id]})
			.expect(function(res) {
				if (res.body.fileIds.length != 1 || !res.body.fileIds[0]) {
					return "invalid files";
				}
				res.body.fileIds = [12];
			})
			.expect(200, {
				status: 'ok',
				fileIds: [12]
			}, done);
	});

	it('/api/user/file/create/ - multiple', function testSlash(done) {
		makeRequest('/api/user/file/create/', {
				numIds: 2,
				friendIds: [sharedFriend._id]})
			.expect(function(res) {
				var fileIds = res.body.fileIds;
				if (fileIds.length != 2 || !fileIds[0] || !fileIds[1]) {
					return "invalid files";
				}
				res.body.fileIds = [12, 13];
			})
			.expect(200, {
				status: 'ok',
				fileIds: [12, 13]
			}, done);
	});

	it('/api/user/file/create/ - multiple string', function testSlash(done) {
		makeRequest('/api/user/file/create/', {
				numIds: '2',
				friendIds: [sharedFriend._id]})
			.expect(function(res) {
				var fileIds = res.body.fileIds;
				if (fileIds.length != 2 || !fileIds[0] || !fileIds[1]) {
					return "invalid files";
				}
				res.body.fileIds = [12, 13];
			})
			.expect(200, {
				status: 'ok',
				fileIds: [12, 13]
			}, done);
	});

	it('/api/user/file/create/ - gobbly gook', function testSlash(done) {
		makeRequest('/api/user/file/create/', {
				numIds: 'abcdefg',
				friendIds: [sharedFriend._id]})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/user/file/create/ - gobbly gook', function testSlash(done) {
		makeRequest('/api/user/file/create/', {
				numIds: '-1	',
				friendIds: [sharedFriend._id]})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/user/file/get/', function testSlash(done) {
		makeRequest('/api/user/file/get/')
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/user/file/get/ - no method, no contentType', function testSlash(done) {
		File.create([sharedUser, sharedFriend], 1).then(function(files) {
			makeRequest('/api/user/file/get/', {
					fileId: files[0].id
				})
				.expect(200, {
					status: 'error'
				}, done);
		});
	});

	it('/api/user/file/get/ - no contentType', function testSlash(done) {
		File.create([sharedUser, sharedFriend], 1).then(function(files) {
			makeRequest('/api/user/file/get/', {
					fileId: files[0].id,
					method: 'post'
				})
				.expect(function(res) {
					assert.notEqual(res.body.fileUrl, null);
					res.body.fileUrl = 'https://s3-url'
				})
				.expect(200, {
					status: 'ok',
					fileUrl: 'https://s3-url'
				}, done);
		});
	});

	it('/api/user/file/get/', function testSlash(done) {
		File.create([sharedUser, sharedFriend], 1).then(function(files) {
			makeRequest('/api/user/file/get/', {
					fileId: files[0].id,
					method: 'post',
					contentType: 'image/jpeg'
				})
				.expect(function(res) {
					assert.notEqual(res.body.fileUrl, null);
					res.body.fileUrl = 'https://s3-url'
				})
				.expect(200, {
					status: 'ok',
					fileUrl: 'https://s3-url'
				}, done);
		});
	});

	it('/api/user/info/update/', function testSlash(done) {
		makeRequest('/api/user/info/update/', {
				firstName: 'Andreas',
				lastName: 'Binnewies'
			})
			.expect(200, {
				status: 'ok'
			})
			.end(function(err, res) {
				User.findById(sharedUser._id).then(function(user) {
					assert.equal(user.first, 'Andreas');
					assert.equal(user.last, 'Binnewies');
					done();
				});
			});
	});

	it('/api/user/info/update/ - Removes whitespace', function testSlash(done) {
		makeRequest('/api/user/info/update/', {
				firstName: '    Andreas2    ',
				lastName: '    Binnewies2    '
			})
			.expect(200, {
				status: 'ok'
			})
			.end(function(err, res) {
				User.findById(sharedUser._id).then(function(user) {
					assert.equal(user.first, 'Andreas2');
					assert.equal(user.last, 'Binnewies2');
					done();
				});
			});
	});

	it('/api/user/info/update/ - first name only', function testSlash(done) {
		makeRequest('/api/user/info/update/', {
				firstName: 'Andreas3',
			})
			.expect(200, {
				status: 'ok'
			})
			.end(function(err, res) {
				User.findById(sharedUser._id).then(function(user) {
					assert.equal(user.first, 'Andreas3');
					assert.equal(user.last, '');
					done();
				});
			});
	});

	it('/api/user/info/get/', function testSlash(done) {
		makeRequest('/api/user/info/get/', {
				userIds: [sharedFriend._id, sharedUser.id]
			})
			.expect(200, {
				names: [
					null, {
						firstName: 'Andreas3',
						lastName: ''
					}
				],
				status: 'ok'
			}, done);
	});

	it('/api/user/info/get/ - no ids', function testSlash(done) {
		makeRequest('/api/user/info/get/')
			.expect(200, {
				names: [],
				status: 'ok'
			}, done);
	});

	it('/api/user/profilepic/', function testSlash(done) {
		makeRequest('/api/user/profilepic/')
			.expect(function(res) {
				assert.notEqual(res.body.uploadUrl, null);
				res.body.uploadUrl = 'https://s3-url'
			})
			.expect(200, {
				status: 'ok',
				uploadUrl: 'https://s3-url'
			}, done);
	});
});
