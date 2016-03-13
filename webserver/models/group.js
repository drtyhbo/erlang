var redis = require('./redis').redis,
	Promise = require('bluebird').Promise;

var Group = function(id) {
	this.id = id;
};
exports.Group = Group;

Group.fields = {
	name: 'name',
	creator: 'creator'
};

// Creates a new group.
Group.create = function(name, user) {
	if (!name || !user) {
		return Promise.reject();
	}

	return Group._create(name, user);
};

Group._create = function(name, user) {
	return redis.incrAsync('group_id').then(function(id) {
		return redis
			.multi()
			.hmset(Group._groupKey(id), Group.fields.name, name, Group.fields.creator, user.id)
			.sadd(Group._membersKey(id), user.id)
			.exec()
			.thenReturn(new Group(id))
	})
}

Group._groupKey = function(id) {
	return 'g:{' + id + '}';
}

Group._membersKey = function(id) {
	return 'gm:{' + id + '}';
}

Group.prototype.addMember = function(user) {
	if (!user) {
		return Promise.reject();
	}

	return redis.saddAsync(Group._membersKey(this.id), user.id);
}

Group.prototype.isMember = function(user) {
	if (!user) {
		return Promise.reject();
	}

	return redis.sismemberAsync(Group._membersKey(this.id), user.id);
}