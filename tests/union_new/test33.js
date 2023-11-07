// @flow

// speculation failure should leave unresolved tvars alone

function foo(cb: (() => string) | (() => number)) {}
function bar() {
  return 0;
}
foo(bar); // error: ambiguous speculation
bar() as string; // error: number ~> string
