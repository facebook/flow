//@flow

var x;
x = null;
x;

var y = null;
y;

var z;
z = null;

function f() {
  z = 42;
}

var w = null;
function g() {
  w = 42;
}

var u = null; // ok
