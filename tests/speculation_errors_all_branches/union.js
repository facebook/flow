/**
 * @format
 * @flow
 */

class X {
  p: number;
}
class Y {
  p: number;
}
class Z {}
class W<T> {
  p: T;
}

type A = number | string;
true as A;

type B = string | {p: string};
42 as B;
({p: 42}) as B;

type C = string | {a: {b: string}} | boolean | {a: {b: boolean}};
42 as C;
({a: {b: 42}}) as C;

type D = string | {a: string} | {a: {b: string}};
42 as D;
({a: 42}) as D;
({a: {b: 42}}) as D;

type E = X | interface {p: string};
42 as E;
new X() as E;
new Y() as E;
new Z() as E;
({p: true}) as E;

type F = interface {p: string} | $ReadOnlyArray<string> | [string, string];
42 as F;
({p: 42}) as F;
({}) as F;
new Y() as F;
new Z() as F;
[1] as [number] as F;
[1, 2] as [number, number] as F;
[1, 2, 3] as [number, number, number] as F;
null as any as Array<number> & {p: number} as F;

type G = string | Z;
42 as G;
({}) as G;

type M = W<string> | interface {p: string};
new W() as W<number> as M;
