var mongoose = require('mongoose');

mongoose.connect('mongodb://localhost/chat', {
    promiseLibrary: require('bluebird')
});

var db = mongoose.connection;

db.on('error', function() {
    console.error('**** error connecting to mongo ****');
});

db.once('open', function() {
    console.log('**** connected to mongo ****');
});
