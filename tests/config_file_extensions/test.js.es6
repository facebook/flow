/*
 * @flow
 */

function foo(x) {
  return x * 10;
}

foo('Hello, world!');

// should find Object.prototype.toString from flowlib
(Object.prototype.toString: Function);
