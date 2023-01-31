// @flow

type F<T> = ?T;
type A = F<string>;
declare var a: A;
a?.length; // okay

type Id<T> = T;
type B = Id<?string>;
declare var b: B;
b?.length; // okay

type C = ?string;
declare var c: C;
c?.length; // okay
