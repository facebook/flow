// @flow

type A = $Call<() => number>;

function foo(x: mixed): %checks {
  return x === 1;
}
declare function bar(x: mixed): boolean %checks(x === 1);
