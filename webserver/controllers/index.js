var express = require('express'),
	router = express.Router();

router.use('/api', require('./api'));

module.exports = router;