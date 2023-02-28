// @flow

// Variable defs and uses
var foo = function() { };
foo();
foo = null;
foo();

// Nested functions
function bar() {
  function bar() {
  }
  bar();
}
bar();

// Classes
class C { }
new C;
class D extends C { }
