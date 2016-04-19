
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

(null + null: 0);
(null + 1: 1);
(1 + null: 1);
(1 + true: 2);
(true + 1: 2);
(1 + false: 1);
(false + 1: 1);
(null + true: 1);
(true + null: 1);
(null + false: 0);
(false + null: 0);

// (we don't do constant propagation with NaN yet)
num(undefined + undefined); // === NaN
num(undefined + 1); // === NaN
num(1 + undefined); // === NaN
num(undefined + true); // === NaN
num(true + undefined); // === NaN

("foo" + true: "footrue");
(true + "foo": "truefoo");
("foo" + null: "foonull");
(null + "foo": "nullfoo");
("foo" + undefined: "fooundefined");
(undefined + "foo": "undefinedfoo");
("foo" + 123: "foo123");
(123 + "foo": "123foo");
("foo" + 123.4: "foo123.4");
(123.4 + "foo": "123.4foo");

(1+1: 2);
("hi " + "there": "hi there");

module.exports = "arith";
