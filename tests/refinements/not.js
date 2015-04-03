/* @flow */

function foo(x: ?bool) {
  if (!x) {
    x++; // should error for null, void and bool (false)
  }
}

function bar(x: ?number) {
  if (!x) {
    x[0]; // should error for null, void and number (0)
  }
}

function baz (x: ?number) {
  if (x === null || x === undefined) {
    return;
  }

  if (!x) {
    x[0]; // should error for number (0)
  }
}
