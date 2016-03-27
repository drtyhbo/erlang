var assert = require('assert'),
	request = require('supertest'),
	redis = require('../models/redis').redis,
	Promise = require('bluebird').Promise,
	Device = require('../models/device').Device,
	User = require('../models/user').User,
	File = require('../models/file').File,
	helpers = require('./test_helpers');

const Constants = {
	phoneNumber: '18315550835',
	friendNumber: '18315551111',
	phoneNumberKey: 'p:{18315550835}',
	deviceUuid: '729908c5a45746af90a88b53a738c218',
	friendDeviceUuid: '8a93c7eaf63a43c881d059ef5c02797f'
};

function createFriend() {
	return User.create(Constants.friendNumber, Constants.friendDeviceUuid).then(function(values) {
		return Promise.resolve(values[0]);
	});
}

describe('logged in', function() {
	var server;
	var sharedUser;
	var sharedDevice;
	var sessionToken;

	before(function (done) {
		server = require('../app');
		helpers.deleteUser(Constants.phoneNumber).then(function() {
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
			done();
		});
	});

	function makeRequest(url, params) {
		params = params || {};
		params['deviceId'] = sharedDevice.id;
		params['session'] = sessionToken;

		return request(server)
			.post(url)
			.send(params);
	}

	it('/api/user/friend/check/', function testSlash(done) {
		var promises = [];
		promises.push(User.create('18315551111'));
		promises.push(User.create('18315552222'));
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

	it('/api/user/friend/prekey/ - no userId', function testSlash(done) {
		makeRequest('/api/user/friend/prekey/')
			.expect(200, {
				status: 'error',
			}, done);
	});

	it('/api/user/friend/prekey/ - no keys', function testSlash(done) {
		makeRequest('/api/user/friend/prekey/', {
				userId: sharedUser.id})
			.expect(200, {
				status: 'error',
			}, done);
	});

	it('/api/user/friend/prekey/', function testSlash(done) {
		return sharedUser.updatePreKeys({
				i: [0, 0xFFFF],
				pk: ['abcd', 'ijkl']})
			.then(function() {
				makeRequest('/api/user/friend/prekey/', {
						userId: sharedUser.id})
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
				sharedDevice.fetch(Device.fields.iosPushToken).then(function(values) {
					assert.equal(values[0], 'pnsToken');
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
				friendId: -1})
			.expect(200, {
				status: 'error'
			}, done);
	});

	it('/api/user/file/create/', function testSlash(done) {
		createFriend().then(function(friend) {
			makeRequest('/api/user/file/create/', {
					numIds: 1,
					friendId: friend.id})
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
	});

	it('/api/user/file/create/ - multiple', function testSlash(done) {
		createFriend().then(function(friend) {
			makeRequest('/api/user/file/create/', {
					numIds: 2,
					friendId: friend.id})
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
	});

	it('/api/user/file/create/ - multiple string', function testSlash(done) {
		createFriend().then(function(friend) {
			makeRequest('/api/user/file/create/', {
					numIds: '2',
					friendId: friend.id})
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
	});

	it('/api/user/file/create/ - gobbly gook', function testSlash(done) {
		createFriend().then(function(friend) {
			makeRequest('/api/user/file/create/', {
					numIds: 'abcdefg',
					friendId: friend.id})
				.expect(200, {
					status: 'error'
				}, done);
			});
	});

	it('/api/user/file/create/ - gobbly gook', function testSlash(done) {
		createFriend().then(function(friend) {
			makeRequest('/api/user/file/create/', {
					numIds: '-1	',
					friendId: friend.id})
				.expect(200, {
					status: 'error'
				}, done);
			});
	});

	it('/api/user/file/get/', function testSlash(done) {
		createFriend().then(function(friend) {
			makeRequest('/api/user/file/get/')
				.expect(200, {
					status: 'error'
				}, done);
		});
	});

	it('/api/user/file/get/ - no method, no contentType', function testSlash(done) {
		createFriend().then(function(friend) {
			return File.create(sharedUser, friend, 1);
		}).then(function(files) {
			makeRequest('/api/user/file/get/', {
					fileId: files[0].id
				})
				.expect(200, {
					status: 'error'
				}, done);
		});
	});

	it('/api/user/file/get/ - no contentType', function testSlash(done) {
		createFriend().then(function(friend) {
			return File.create(sharedUser, friend, 1);
		}).then(function(files) {
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
		createFriend().then(function(friend) {
			return File.create(sharedUser, friend, 1);
		}).then(function(files) {
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
				sharedUser.fetch(User.fields.firstName, User.fields.lastName).then(function(values) {
					assert.equal(values[0], 'Andreas');
					assert.equal(values[1], 'Binnewies');
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
				sharedUser.fetch(User.fields.firstName, User.fields.lastName).then(function(values) {
					assert.equal(values[0], 'Andreas2');
					assert.equal(values[1], 'Binnewies2');
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
				sharedUser.fetch(User.fields.firstName).then(function(values) {
					assert.equal(values[0], 'Andreas3');
					done();
				});
			});
	});

	it('/api/user/info/update/ - last name only', function testSlash(done) {
		makeRequest('/api/user/info/update/', {
				lastName: 'Binnewies3',
			})
			.expect(200, {
				status: 'ok'
			})
			.end(function(err, res) {
				sharedUser.fetch(User.fields.lastName).then(function(values) {
					assert.equal(values[0], 'Binnewies3');
					done();
				});
			});
	});

	it('/api/user/info/get/', function testSlash(done) {
		User.create('18315551111').then(function(friend) {
			makeRequest('/api/user/info/get/', {
					userIds: [friend.id, sharedUser.id]
				})
				.expect(200, {
					names: [
						null, {
							firstName: 'Andreas3',
							lastName: 'Binnewies3'
						}
					],
					status: 'ok'
				}, done);
		});
	});

	it('/api/user/info/get/ - no ids', function testSlash(done) {
		User.create('18315551111').then(function(friend) {
			makeRequest('/api/user/info/get/')
				.expect(200, {
					names: [],
					status: 'ok'
				}, done);
		});
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