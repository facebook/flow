function test_0() {
  declare function g<F>(f: F): boolean;
  declare var f: mixed;
  if(g<typeof f>(f)) {} // 'typeof f' should be resolved
}

function f2() {
  declare function g<F: any>(f: F, x: mixed): boolean %checks(f(x));
  declare function f(x: mixed): boolean %checks(typeof x === "number");

  if(g<typeof f>(f)) {} // 'typeof f' should be resolved
}
