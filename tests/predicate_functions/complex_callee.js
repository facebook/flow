declare var x: mixed;
declare function f(x: mixed): boolean %checks(typeof x === "number");

function test_0() {
  if((() => f)()(x)) {
    (x: number);
    (x: string); // error
  }
}

function test_1() {
  const a = [f];
  if (a[0](x)) {
    (x: number);
    (x: string); // error
  }
}
