// @flow

function test(a: string, b: number): number {
  return this.length; // expect []/"" this
}

// args flow correctly into params
test.call("", "", 0);

// wrong this is an error
test.call(0, "", 0); // error: lookup `length` on Number

// not enough arguments is an error (via incompatible RestT)
test.call("", ""); // error: string ~> number

// mistyped arguments is an error
test.call("", "", ""); // error: string ~> number (2nd arg)
test.call("", 0, 0); // error: number ~> string (1st arg)

// resolve args array from tvar
function f(args) { test.call("", args[0], args[1]) }
f(["", 0]); // OK
f(["", ""]); // error: string ~> number (2nd arg)
f([0, 0]); // error: number ~> string (1st arg)
