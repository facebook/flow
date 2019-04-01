/* @providesModule Arith */

function num(x: number) {}

function str(x: string) {}

function bignum(x: bigint) {}

function foo() {
  var w = 0n;
  var x = 0;
  var y = "...";
  var z = {};
  bignum(w + w);
  bignum(w + x); // error
  bignum(w + y); // error
  num(x + x);
  num(x + y); // error
  str(x + y);
  str(x + x); // error
  str(z + y); // error
}

// test MaybeT(NumT)
function bar0(x: ?number, y: number) {
  num(x + y);
}
function bar1(x: number, y: ?number) {
  num(x + y);
}

// test MaybeT(BigNumT)
function bigint_bar0(x: ?bigint, y: bigint) {
  bignum(x + y);
}
function bigint_bar1(x: bigint, y: ?bigint) {
  bignum(x + y);
}

// test OptionalT(NumT)
function bar2(x?: number, y: number) {
  num(x + y);
}
function bar3(x: number, y?: number) {
  num(x + y);
}

// test OptionalT(BigNumT)
function bigint_bar2(x?: bigint, y: bigint) {
  bignum(x + y);
}
function bigint_bar3(x: bigint, y?: bigint) {
  bignum(x + y);
}

// test OptionalT(MaybeT(NumT))
function bar4(x?: ?number, y: number) {
  num(x + y);
}
function bar5(x: number, y?: ?number) {
  num(x + y);
}

// test OptionalT(MaybeT(BigNumT))
function bigint_bar4(x?: ?bigint, y: bigint) {
  bignum(x + y);
}
function bigint_bar5(x: bigint, y?: ?bigint) {
  bignum(x + y);
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

bignum(null + 1n); // error
bignum(1n + null); // error
bignum(undefined + 1n); // error
bignum(1n + undefined); // error

str("foo" + true); // error
str(true + "foo"); // error
str("foo" + null); // error
str(null + "foo"); // error
str("foo" + undefined); // error
str(undefined + "foo"); // error

let tests = [
  function(x: mixed, y: mixed) {
    x + y; // error
    x + 0; // error
    0 + x; // error
    x + 0n; // error
    0n + x; // error
    x + ""; // error
    "" + x; // error
    x + {}; // error
    ({} + x); // error
  },

  // when one side is a string or number and the other is invalid, we
  // assume you are expecting a string or number (respectively), rather than
  // erroring twice saying number !~> string and obj !~> string.
  function() {
    (1 + {}: number); // error: object !~> number
    ({} + 1: number); // error: object !~> number
    (1n + {}: bigint); // error: object !~> bigint
    ({} + 1n: bigint); // error: object !~> bigint
    ("1" + {}: string); // error: object !~> string
    ({} + "1": string); // error: object !~> string
  },

  function(x: any, y: number, z: string) {
    (x + y: string); // ok
    (y + x: string); // ok

    (x + z: empty); // error, string ~> empty
    (z + x: empty); // error, string ~> empty
  },

  function(x: number, y: number) {
    (x + y).length; // error
    (1 + 2).length; // error
  }
];
