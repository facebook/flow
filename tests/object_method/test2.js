/* @flow */

function f(this: {p: mixed}) {
  return this.p;
}

var a = {
  p: 0,
  f
}

var b = {
  f
}

a.f(); // okey-dokie
b.f(); // error, property `p` not found
