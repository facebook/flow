//@flow

var x = []
x.push(42);
(x[0]: empty); /// error from num and str
x.push('a'); // error
x = [10]; // ok
x = 100; // error

var y = []
y = 100; // error

var z = []
function f() { z = [1] } // error, selected str
z.push('a');

var w = []
w.push([42]);
(w: Array<Array<?number>>) // unfortunate error

var u = [];
u[0] = 'a';
u.push(42); // error

function foo() {
  var xin = [];
  xin.push(42);
  xin.push('a') // error
}

var v: ?Array<number> = [];
v = []; // no annotation needed
if (v == null) {
  v = []; // no annotation needed
}
(v: Array<number>);
