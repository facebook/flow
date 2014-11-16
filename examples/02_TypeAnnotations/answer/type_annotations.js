/* @flow */

// Changing the return type to number fixes the error
function foo(x: string, y: number): number {
  return x.length * y;
}

foo("Hello", 42);
