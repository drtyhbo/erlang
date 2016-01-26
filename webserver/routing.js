var app = require('app').app;

app.get('/register/', function(request, response)) {
	response.send('test');
});