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

// setImmediate should run clear its queued cbs once per event loop turn
// but immediates queued while processing the current queue should happen
// on the next turn of the event loop.

// in v0.10 hit should be 1, because we only process one cb per turn
// in v0.11 and beyond it should be the exact same size of QUEUE
// if we're letting things recursively add to the immediate QUEUE hit will be
// > QUEUE

var ticked = false;

var hit = 0;
var QUEUE = 1000;

function run() {
  if (hit === 0)
    process.nextTick(function() { ticked = true; });

  if (ticked) return;

  hit += 1;
  setImmediate(run);
}

for (var i = 0; i < QUEUE; i++)
  setImmediate(run);

process.on('exit', function() {
  console.log('hit', hit);
  assert.strictEqual(hit, QUEUE, 'We ticked between the immediate queue');
});
