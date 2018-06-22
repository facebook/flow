// @flow

type A = A;
type B = B | null;
type C = A;
type D = { x: number };
type E = $Exact<D>;
type F = ?D;

type P<T: string> = { t: T } | boolean;
declare var a: P<string>;
if (typeof a !== "boolean") a;

function f<X>() {
  type A = X;
  type B = B | null;
  type C = A;
  type D = { x: X };
  type E = $Exact<X>;
  type F = ?X;
}
