
/* @providesModule Arith */

function num(x:number) { }

function str(x:string) { }

function foo() {
  var x = 0;
  var y = "...";
  var z = {};
  num(x+x);
  num(x+y); // error
  str(x+y);
  str(x+x); // error
  str(z+y); // error

  // 3 errors: x+z is a string, but
  //   1) num !~> string
  //   2) object !~> string
  //   3) string ~> num (function call)
  num(x+z);
}

// test MaybeT(NumT)
function bar0(x: ?number, y: number) {
  num(x + y);
}
function bar1(x: number, y: ?number) {
  num(x + y);
}

// test OptionalT(NumT)
function bar2(x?: number, y: number) {
  num(x + y);
}
function bar3(x: number, y?: number) {
  num(x + y);
}

// test OptionalT(MaybeT(NumT))
function bar4(x?: ?number, y: number) {
  num(x + y);
}
function bar5(x: number, y?: ?number) {
  num(x + y);
}

num(null + null); // === 0
num(undefined + undefined); // === NaN

num(null + 1); // === 1
num(1 + null); // === 1
num(undefined + 1); // === NaN
num(1 + undefined); // === NaN

num(null + true); // === 1
num(true + null); // === 1
num(undefined + true); // === NaN
num(true + undefined); // === NaN

str("foo" + true); // error
str(true + "foo"); // error
str("foo" + null); // error
str(null + "foo"); // error
str("foo" + undefined); // error
str(undefined + "foo"); // error

module.exports = "arith";
