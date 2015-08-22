/* @flow */

function foo(x, y) {
  "use strict";
  return bar(x) + baz(y);

  // hoisted, should not generate warning
  function bar (ecks) {
    return x + ecks;
  }

  // assignment not hoisted, should generate warning
  var baz = function (why) {
    return y + why;
  };

}

foo(1, 2);
