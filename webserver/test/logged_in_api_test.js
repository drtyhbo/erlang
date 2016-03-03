var assert = require('assert'),
	request = require('supertest'),
	redis = require('../models/redis').redis,
	Promise = require('bluebird').Promise,
	User = require('../models/user').User,
	helpers = require('./test_helpers');

const Constants = {
	phoneNumber: '18315550835',
	phoneNumberKey: 'p:{18315550835}'
};

describe('logged in', function() {
	var server;
	var sharedUser;
	var sessionToken;

	before(function (done) {
		server = require('../app');
		helpers.deleteUser(Constants.phoneNumber).then(function() {
			return User.create(Constants.phoneNumber);
		}).then(function(user) {
			sharedUser = user;
			return user.fetch(User.fields.code);
		}).then(function(values) {
			return User.login(Constants.phoneNumber, values[0]);
		}).then(function(user) {
			return user.fetch(User.fields.session);
		}).then(function(values) {
			sessionToken = values[0]
			done();
		});
	});

	function makeRequest(url, params) {
		params = params || {};
		params['id'] = sharedUser.id;
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

	it('/api/user/friend/prekey/ - no token', function testSlash(done) {
		makeRequest('/api/user/pns/register/', {
				userId: sharedUser.id})
			.expect(200, {
				status: 'error'
			}, done);
	});


	it('/api/user/friend/prekey/', function testSlash(done) {
		makeRequest('/api/user/pns/register/', {
				userId: sharedUser.id,
				token: 'pnsToken'})
			.expect(200, {
				status: 'error'
			})
			.end(function(err, res) {
				sharedUser.fetch(User.fields.iosPushToken).then(function(values) {
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
		User.create('18315551111').then(function(friend) {
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
		User.create('18315551111').then(function(friend) {
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
});