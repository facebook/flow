//@flow

var x = 42;
x = "a";

function f() {
  return 42;
}

function g() {
  return "a";
}

var y = f();
y = g();

var w = null;
w = 42;
w = "a";
