var mongoose = require('mongoose');

mongoose.connect('mongodb://chat1.drtyhbo.com,chat2.drtyhbo.com/chat', {
    promiseLibrary: require('bluebird'),
    replSet: {
        connectWithNoPrimary: true,
        rs_name: 'rs0',
        ssl: true,
        sslValidate: false
    },
    user: 'chat',
    pass: 'u*hw{//B87}YGU='
});

var db = mongoose.connection;

db.on('error', function(e) {
    console.error('**** error connecting to mongo: ' + e.message + ' ****');
});

db.once('open', function() {
    console.log('**** connected to mongo ****');
});
