// @flow

type A = $Call<() => number>;
type B = $ObjMap<{}, () => number>;
type C = $ObjMapi<{}, () => number>;
type D = $ObjMapConst<{}, number>;

function foo(x: mixed): %checks {
  return x === 1;
}
declare function bar(x: mixed): boolean %checks(x === 1);
