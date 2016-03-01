var express = require('express'),
    bodyParser = require('body-parser'),
    morgan = require('morgan');

app = express();

app.use(morgan('combined'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({
    extended: true
}));
app.use(require('./controllers'));

var server = app.listen(8080, function() {

});
module.exports = server;