/* @flow */

function foo(x) {
  return x.length;
}

var res = foo("Hello") + foo(42);
