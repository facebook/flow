/* @flow */

function foo(x: number, y: number) {
  "use strict";
  return bar(x) + baz(y);

  // function declaration is hoisted, should not generate warning
  function bar (ecks: number) {
    return x + ecks;
  }

  // assignment is not hoisted, should generate warning
  var baz = function (why: number) {
    return y + why;
  };

  // variable declaration is hoisted, should not generate warning
  var x, y, z;

  // assignments are not hoisted, should generate 2 warnings
  var t,
      u = 5,
      v,
      w = 7;
}

foo(1, 2);

// Regression tests for new-env crash.
function unreachable_writes() {
  () => {
    return;
    { let i = 0; i = 1; } // error: unreachable
  };
  () => {
    return;
    { let i = 0; i++; } // error: unreachable
  };
}
