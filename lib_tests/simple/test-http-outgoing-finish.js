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

var common = require('../common');
var assert = require('assert');

var http = require('http');

http.createServer(function(req, res) {
  req.resume();
  req.on('end', function() {
    write(res);
  });
  this.close();
}).listen(common.PORT, function() {
  var req = http.request({
    port: common.PORT,
    method: 'PUT'
  });
  write(req);
  req.on('response', function(res) {
    res.resume();
  });
});

var buf = new Buffer(1024 * 16);
buf.fill('x');
function write(out) {
  var name = out.constructor.name;
  var finishEvent = false;
  var endCb = false;

  // first, write until it gets some backpressure
  while (out.write(buf)) {}

  // now end, and make sure that we don't get the 'finish' event
  // before the tick where the cb gets called.  We give it until
  // nextTick because this is added as a listener before the endcb
  // is registered.  The order is not what we're testing here, just
  // that 'finish' isn't emitted until the stream is fully flushed.
  out.on('finish', function() {
    finishEvent = true;
    console.error('%s finish event', name);
    process.nextTick(function() {
      assert(endCb, name + ' got finish event before endcb!');
      console.log('ok - %s finishEvent', name);
    });
  });

  out.end(buf, function() {
    endCb = true;
    console.error('%s endCb', name);
    process.nextTick(function() {
      assert(finishEvent, name + ' got endCb event before finishEvent!');
      console.log('ok - %s endCb', name);
    });
  });
}
