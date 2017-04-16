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

var stream = require('stream');
var util = require('util');

function MyWritable(fn, options) {
  stream.Writable.call(this, options);
  this.fn = fn;
};

util.inherits(MyWritable, stream.Writable);

MyWritable.prototype._write = function (chunk, encoding, callback) {
  this.fn(Buffer.isBuffer(chunk), typeof chunk, encoding);
  callback();
};

;(function decodeStringsTrue() {
  var m = new MyWritable(function(isBuffer, type, enc) {
    assert(isBuffer);
    assert.equal(type, 'object');
    assert.equal(enc, 'buffer');
    console.log('ok - decoded string is decoded');
  }, { decodeStrings: true });
  m.write('some-text', 'utf8');
  m.end();
})();

;(function decodeStringsFalse() {
  var m = new MyWritable(function(isBuffer, type, enc) {
    assert(!isBuffer);
    assert.equal(type, 'string');
    assert.equal(enc, 'utf8');
    console.log('ok - un-decoded string is not decoded');
  }, { decodeStrings: false });
  m.write('some-text', 'utf8');
  m.end();
})();
