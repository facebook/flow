// @flow

var x = 42; // number | string
x = "a";

var y = null; // no change
y = 42;

var z = null; // null | number | string
z = 42;
z = "a";

var w = 42; // number | bool, don't take on upper bound
w = true;
function ub(x: string) { }
ub(w);

class A { }
class B extends A { }
class C extends B { }

var x1 = new B(); // no change
x1 = new C();

var x2 = new C(); // C | B
x2 = new B();

var x3; // no change
if (w) {
  x3 = 42;
} else {
  x3 = "hello"
}

var x4; // number | string | boolean
if (w) {
  x4 = 42;
} else {
  x4 = "hello"
}
x4 = true;

var x5; // number | string | boolean
x5 = true;
if (w) {
  x5 = 42;
} else {
  x5 = "hello"
}
x5;

declare var union: number | string | boolean;

var x6 = 42; // number | string | boolean
x6 = "a";
x6 = union;

var x7 = 42; // ?number
declare var x7a: ?number;
x7 = x7a;

var x8: number = 42 // no change (already annotated as number)
x8 = "hello";

var x9 = 42;
if (w) {
  x9 = "hello"
}
x9;
