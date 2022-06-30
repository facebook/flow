/* @flow */

function foo(x: ?number): string {
  if (x) {
    // Distributed checking is a no-op, no error expected
    return x;
  }
  return "default string";
}
