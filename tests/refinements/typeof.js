/* @flow */

function foo(x: bool | number) {
  if (typeof x === "boolean") {
    x[0]; // error for boolean, not number
  }
}

function bar(): number {
  var x = null;
  if (typeof x === "object") {
    return x; // error, null
  }
  return 0;
}
