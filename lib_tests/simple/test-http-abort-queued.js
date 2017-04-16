// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

var assert = require('assert'),
    http = require('http');

var complete;

var server = http.createServer(function (req, res) {
  // We should not see the queued /thatotherone request within the server
  // as it should be aborted before it is sent.
  assert.equal(req.url, '/');

  res.writeHead(200);
  res.write('foo');

  complete = complete || function () {
    res.end();
  };
});


server.listen(0, function () {
  console.log('listen', server.address().port);

  var agent = new http.Agent({maxSockets: 1});
  assert.equal(Object.keys(agent.sockets).length, 0);

  var options = {
    hostname: 'localhost',
    port: server.address().port,
    method: 'GET',
    path: '/',
    agent: agent
  };

  var req1 = http.request(options);
  req1.on('response', function(res1) {
    assert.equal(Object.keys(agent.sockets).length, 1);
    assert.equal(Object.keys(agent.requests).length, 0);

    var req2 = http.request({
      method: 'GET',
      host: 'localhost',
      port: server.address().port,
      path: '/thatotherone',
      agent: agent
    });
    assert.equal(Object.keys(agent.sockets).length, 1);
    assert.equal(Object.keys(agent.requests).length, 1);

    req2.on('error', function(err) {
      // This is expected in response to our explicit abort call
      assert.equal(err.code, 'ECONNRESET');
    });

    req2.end();
    req2.abort();

    assert.equal(Object.keys(agent.sockets).length, 1);
    assert.equal(Object.keys(agent.requests).length, 1);

    console.log('Got res: ' + res1.statusCode);
    console.dir(res1.headers);

    res1.on('data', function(chunk) {
      console.log('Read ' + chunk.length + ' bytes');
      console.log(' chunk=%j', chunk.toString());
      complete();
    });

    res1.on('end', function() {
      console.log('Response ended.');

      setTimeout(function() {
        assert.equal(Object.keys(agent.sockets).length, 0);
        assert.equal(Object.keys(agent.requests).length, 0);

        server.close();
      }, 100);
    });
  });

  req1.end();
});
