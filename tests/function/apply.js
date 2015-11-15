function test(a: string, b: number): number {
  return this.length; // expect []/"" this
}

// tuples flow correctly into params
test.apply("", ["", 0]);

// wrong this is an error
test.apply(0, ["", 0]); // error: lookup `length` on Number

// not enough arguments is an error (via incompatible RestT)
test.apply("", [""]); // error: string ~> number

// mistyped arguments is an error
test.apply("", ["", ""]); // error: string ~> number (2nd arg)
test.apply("", [0, 0]); // error: number ~> string (1st arg)

// resolve args array from tvar
function f(args) { test.apply("", args) }
f(["", 0]); // OK
f(["", ""]); // error: string ~> number (2nd arg)
f([0, 0]); // error: number ~> string (1st arg)

// expect array
test.apply("", "not array"); // error: expect array of args
