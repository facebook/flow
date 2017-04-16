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
var cluster = require('cluster');

assert(cluster.isMaster);

var assertsRun = 0;

function emitAndCatch(next) {
  cluster.once('setup', function(settings) {
    assert.strictEqual(settings.exec, 'new-exec');
    console.log('ok "setup" emitted with options set');
    assertsRun += 1;
    setImmediate(next);
  });
  cluster.setupMaster({ exec: 'new-exec' });
}

function emitAndCatch2(next) {
  cluster.once('setup', function(settings) {
    assert('exec' in settings);
    console.log('ok "setup" emitted without options set');
    assertsRun += 1;
    setImmediate(next);
  });
  cluster.setupMaster();
}

process.on('exit', function() {
  assert.strictEqual(assertsRun, 2);
  console.log('ok correct number of assertions');
});

emitAndCatch(function() {
  emitAndCatch2(function() {
    console.log('ok emitted and caught');
  });
});
