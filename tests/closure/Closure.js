
/* @providesModule Closure */

function is_string(_:string) { }

function f() { }

var x = "hello";
f();
is_string(x);
x = 42;

function g() {
  var y = "hello";
  f();
  is_string(y);
  y = 42;
}

var o = { f: function() { } }

function h() {
  var z = "hello";
  o.f();
  is_string(z);
  z = 42;
}

var w = "hello";
o.f();
is_string(w);
w = 42;

module.exports = true;
