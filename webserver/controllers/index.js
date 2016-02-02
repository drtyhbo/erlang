var express = require('express'),
	router = express.Router();

router.use('/api/user', require('./logged_in_api'));
router.use('/api', require('./logged_out_api'));

module.exports = router;