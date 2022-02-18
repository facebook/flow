// @flow

var x: ?number = 0;
function foo() {
  bar();
}
function bar() {
  x = null;
}
if (x != null) {
  foo();
  (x: number);
}