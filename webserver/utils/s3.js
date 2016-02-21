var sig = require('amazon-s3-url-signer');

var fileBucket = sig.urlSigner('AKIAIRKB5XME5BRBKYWQ', 'mC2r6bNkM6AaxWqBO6iJ3enL6yACsbOjOEvWrwYv', {
	useSubdomain: true,
	protocol: "https"});

exports.generateSignedUrl = function(method, path, bucket, contentType) {
	return fileBucket.getUrl(method, path, bucket, contentType || '', 1);
}