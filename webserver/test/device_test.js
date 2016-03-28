/*	it('User - checkPreKeys none', function testSlash(done) {
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

	*/