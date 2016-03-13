var assert = require('assert'),
	request = require('supertest'),
	redis = require('../models/redis').redis,
	Promise = require('bluebird').Promise,
	Group = require('../models/group').Group,
	User = require('../models/user').User,
	helpers = require('./test_helpers');

const Constants = {
	phoneNumber: '18315550835',
};

describe('group', function() {
	var sharedUser;

	before(function (done) {
		helpers.deleteUser(Constants.phoneNumber).then(function() {
			return User.create(Constants.phoneNumber);
		}).then(function(user) {
			sharedUser = user;
			done();
		});
	});

	it('create - no name', function testSlash(done) {
		Group.create().then(function() {
		}, function() {
			done();
		});
	});

	it('create - no user', function testSlash(done) {
		Group.create('testGroup').then(function() {
		}, function() {
			done();
		});
	});

	it('create', function testSlash(done) {
		Group.create('testGroup', sharedUser).then(function(group) {
			assert.notEqual(group, null);
			return group.isMember(sharedUser);
		}).then(function(isMember) {
			assert.equal(isMember, true);
			done()
		});
	});

	it('add - no user', function testSlash(done) {
		Group.create('testGroup', sharedUser).then(function(group) {
			return group.addMember();
		}).then(function() {
		}, function() {
			done();
		});
	});

	it('add - existing user', function testSlash(done) {
		Group.create('testGroup', sharedUser).then(function(group) {
			return group.addMember(sharedUser);
		}).then(function() {	
			return group.isMember(sharedUser);
		}).then(function(isMember) {
			assert.equal(isMember, true);
		}, function() {
			done();
		});
	});

	it('add', function testSlash(done) {
		var sharedGroup;
		Group.create('testGroup', sharedUser).then(function(group) {
			sharedGroup = group;
			return User.create('18315551111');
		}).then(function(newUser) {	
			return sharedGroup.addMember(newUser).thenReturn(newUser);
		}).then(function(newUser) {
			return sharedGroup.isMember(newUser);
		}).then(function(isMember) {
			assert.equal(isMember, true);
			done();
		});
	});
});