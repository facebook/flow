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

/* refining globals */
function fn0() {
  if (typeof BAZ !== 'undefined' &&
      typeof BAZ.stuff === 'function') {
    BAZ.stuff(123);
  }
  BAZ.stuff(123); // error, refinement is gone
}
function fn1() {
  BAZ.stuff; // error, could be undefined
  if (typeof BAZ !== 'undefined' &&
      typeof BAZ.stuff === 'function') {
    BAZ.stuff(123); // ok
    BAZ.stuff(123); // error, refinement is gone
  }
}
