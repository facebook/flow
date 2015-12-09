/*
 * @flow
 */

function foo(x) {
  return x * 10;
}

foo('Hello, world!');

require('./test');

// should find Object.prototype.toString from flowlib
(Object.prototype.toString: Function);
