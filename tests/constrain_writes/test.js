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


function ff2(x) {
  x = "Hello";
  x = 42; // no failure, parameters don't have post-inference checks
}
ff2(true); // give x a lb

function pos() {
  class X {foo: number;};
  const x = new X;
  var a : string;
  var {...a} = x; // no post-check here
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
