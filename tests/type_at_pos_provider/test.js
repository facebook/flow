// @flow

var x = 42;
//  ^
  x = "hello world";
//^
var y = null;
function f() {
  y = 42;
//^
}
  y = "hello world";
//^
  z = "hello world";
//^
var z = 42;
//  ^
  x = 42;
//^
var { a } = { a: x };
//    ^
  a = 'hello';
//^
