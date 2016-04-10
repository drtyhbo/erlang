var mongoose = require('mongoose'),
	utils = require('../utils/utils.js'),
	Device = require('../models/device.js').Device,
	Promise = require('bluebird').Promise;

var userSchema = mongoose.Schema({
	phone: String,
	first: String,
	last: String
});

userSchema.methods.getActiveDevice = function() {
	return Device.findDevicesForUser(this).then(function(devices) {
		return Promise.resolve(devices[0]);
	});
};

var User = mongoose.model('User', userSchema);

User.checkPhoneNumbers = function(phoneNumbers) {
	return User.find({
		phone: { $in: phoneNumbers }
	}).then(function(users) {
		// Return the list of users in the order they were provided in phoneNumbers
		// with missing users replaced with null.
		var results = [];
		for (var i = 0; i < phoneNumbers.length; i++) {
			var user = User._userWithPhoneNumber(phoneNumbers[i], users);
			results.push(user)
		}

		return Promise.resolve(results);
	});
};

User._userWithPhoneNumber = function(phoneNumber, users) {
	for (var i = 0; i < users.length; i++) {
		if (phoneNumber == users[i].phone) {
			return users[i];
		}
	}
	return null;
};

User.create = function(phoneNumber, deviceUuid) {
	if (!phoneNumber || phoneNumber.length != 11) {
		return Promise.reject();
	}

	var sharedUser;
	var sharedDevice;
	return this._create(phoneNumber).then(function(user) {
		sharedUser = user;
		return Device.create(deviceUuid, user._id);
	}).then(function(device) {
		sharedDevice = device;
		return device.generateCode();
	}).then(function(code) {
		return Promise.resolve([sharedUser, sharedDevice, code]);
	});
};

User.findUser = function(phoneNumber) {
	return User.find({ phone: phoneNumber }).then(User._findUserCallback);
};

User.findUserById = function(userId) {
	return User.find({ _id: userId }).then(User._findUserCallback);
};

User.findUsersByIds = function(userIds) {
	return User.find({ _id: { $in: userIds }});
};

User._findUserCallback = function(users) {
	if (!users.length) {
		return Promise.reject();
	} else {
		return Promise.resolve(users[0]);
	}
}

User.verifyNumber = function(phoneNumber, deviceUuid, code) {
	var sharedUser;
	var sharedDevice;

	return User.findUser(phoneNumber).then(function(user) {
		sharedUser = user;
		return Device.findDevice(deviceUuid);
	}).then(function(device) {
		if (device.userId.toString() != sharedUser._id.toString() || !code || device.code != code) {
			return Promise.reject();
		}

		return Promise.resolve([sharedUser, device]);
	});
};

User._create = function(phoneNumber) {
	return User.find({
		phone: phoneNumber
	}).then(function(users) {
		if (!users.length) {
			var user = new User({
				phone: phoneNumber
			});
			return user.save();
		} else {
			return Promise.resolve(users[0]);
		}
	});
};

exports.User = User;


// Checks whether the provided numbers correspond to users and returns an array of the
// phoneNumber/id combinations.
/*



// Resolves to an array with the following members:
// 0 - The user object.
// 1 - The device object.
User.verifyNumber = function(phoneNumber, deviceUuid, code) {
	var sharedUser;
	var sharedDevice;
	return User._getUserId(phoneNumber).then(function(id) {
		if (!id) {
			return Promise.reject();
		}

		sharedUser = new User(id);
		return sharedUser.findDevice(deviceUuid);
	}).then(function(device) {
		sharedDevice = device;
		return device.fetch(Device.fields.code);
	}).then(function(values) {
		if (!code || values[0] != code) {
			return Promise.reject();
		}
		return sharedUser
			._update(User.fields.active, true)
			.thenReturn([sharedUser, sharedDevice]);
	});
};

User._create = function(phoneNumber) {
	//var code = Math.round(Math.random() * 899999) + 100000;
	var code = 111111;
	return redis.incrAsync('user_id').then(function(id) {
		return redis.setAsync(User._phoneKey(phoneNumber), id).thenReturn(id);
	}).then(function(id) {
		return redis.hmsetAsync(User._userKey(id), [User.fields.phone, phoneNumber]).thenReturn(new User(id));
	});
}

User._getUserId = function(phoneNumber) {
	return redis.getAsync(User._phoneKey(phoneNumber));
};

User._phoneKey = function(phoneNumber) {
	return 'p:{' + phoneNumber + '}';
};

User._userKey = function(id) {
	return 'u:{' + id + '}';
};

User._devicesKey = function(id) {
	return 'ud:{' + id + '}';
};

User.prototype.exists = function() {
	return redis.existsAsync(User._userKey(this.id));
};

User.prototype.fetch = function() {
	return redis.hmgetAsync(User._userKey(this.id), Array.prototype.slice.call(arguments));
};

User.prototype.findDevice = function(deviceUuid) {
	var self = this;
	return redis.hgetAsync(User._devicesKey(this.id), deviceUuid).then(function(deviceId) {
		if (deviceId) {
			return Promise.resolve(deviceId ? new Device(deviceId) : null);
		} else {
			return self._createDevice(deviceUuid);
		}
	});
};

User.prototype.getActiveDevice = function() {
	return redis.hgetallAsync(User._devicesKey(this.id)).then(function(devices) {
		for (deviceUuid in devices) {
			return Promise.resolve(devices[deviceUuid]);
		}
		return Promise.reject();
	});
};

User.prototype.update = function() {
	if (arguments[0] instanceof Array) {
		var itemsToUpdate = arguments[0];
	} else {
		var itemsToUpdate = Array.prototype.slice.call(arguments);
	}

	for (var i = itemsToUpdate.length - 2; i >= 0; i -= 2) {
		var fieldName = itemsToUpdate[i];
		if (fieldName != User.fields.firstName && fieldName != User.fields.lastName && fieldName != User.fields.iosPushToken) {
			itemsToUpdate.splice(i, 2);
		}
	}

	return this._update.apply(this, itemsToUpdate);
};

User.prototype._createDevice = function(deviceUuid) {
	var self = this;
	return Device.create(this.id).then(function(device) {
		return redis.hsetAsync(User._devicesKey(self.id), deviceUuid, device.id).thenReturn(device);
	});
};

User.prototype._update = function() {
	return redis.hmsetAsync(User._userKey(this.id), Array.prototype.slice.call(arguments));
};
*/
