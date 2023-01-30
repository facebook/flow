/**
 * @format
 * @flow
 */

const tup: [number] = [42];
tup[(0: number)] = 123; // error

function foo(x: [1, 2]) {
  x[2]; // error
  x[-1]; // error
}

// Unknown key access returns the general type
function bar(x: [1], y: number): string {
  return x[y]; // error: number ~> string
}
