// @flow
async function foo() {
  return 42;
}

async function bar() {
  var a = await foo();
  var b: number = a; // valid
  var c: string = a; // Error: number ~> string
}
