var express = require('express'),
    bodyParser = require('body-parser');

app = express();

app.use(bodyParser.urlencoded({
    extended: true
}));
app.use(require('./controllers'));

app.listen(3000, function() {

});