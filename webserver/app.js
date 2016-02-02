var express = require('express'),
    bodyParser = require('body-parser'),
    morgan = require('morgan');

app = express();

app.use(morgan('combined'));
app.use(bodyParser.urlencoded({
    extended: true
}));
app.use(require('./controllers'));

app.listen(3000, function() {

});