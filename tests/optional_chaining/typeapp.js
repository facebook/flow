// @flow

type F<T> = ?T;
type A = F<string>;
declare const a: A;
a?.length; // okay

type Id<T> = T;
type B = Id<?string>;
declare const b: B;
b?.length; // okay

type C = ?string;
declare const c: C;
c?.length; // okay
