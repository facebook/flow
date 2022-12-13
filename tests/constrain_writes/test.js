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

var z;
z = 42;
z = "a";

function ff(x: number) {
  x = "Hello"; // only regular failure
}

function pos() {
  class X {foo: number;};
  const x = new X;
  var a : string;
  ({...a} = x);
}

function destruct() {
  var {p}: T = {p: "foo"};
  p = 42; // error: number ~> string, but no post-infer
  type T = {p: string};
}

function ifs(x: mixed) {
  let a = "";
  if (x) {
      let a;
      a = 0; // doesn't add lower bound to outer a
  }
  (a : string); // OK
}


function switch_scope(x: mixed) {
  let a = "";
  switch (x) {
    case "foo":
      let a;
      a = 0; // doesn't add lower bound to outer a
  }
  (a : string); // OK
}

function trycatch() {
  try {
  } catch (e) {
    e = 42;
  }
}

function classexp() {
  let _ = class e {
    f() {
      e = 42;
    }
  }
}

function decl() {
  declare var x: number;
  declare function f(): void;

  x = "hi";
  f = "hi";
}

function interference_post_inference_check() {
  // A regression test to check that post inference checks do not interfere with each other.
  let x = []; // error
  x = [3];
  x.length === 1 ? 1 : 2;
}
