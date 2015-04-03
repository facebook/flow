/* @flow */

function foo(x: bool | number) {
  if (typeof x === "boolean") {
    x[0]; // error for boolean, not number
  }
}
