/* @flow */

function foo(x) {
  if (typeof(x) === 'string') {
    return x.length;
  } else {
    return x;
  }
}

var res = foo("Hello") + foo(42);
