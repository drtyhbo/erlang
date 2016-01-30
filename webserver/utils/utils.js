var crypto = require('crypto');

exports.generateSessionToken = function() {
    var sha = crypto.createHash('sha256');
    sha.update(Math.random().toString());
    return sha.digest('hex');
};