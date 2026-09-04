// @flow

declare function f(x: number): string;
declare function f(x: string): string;
declare function f(x: boolean): string;

f(1);
f('hi');

// A reference that is not a call keeps the whole intersection, so it needs no
// count.
const g = f;

declare function solo(x: number): string;
solo(1);

declare class C {
  m(x: number): string;
  m(x: string): string;
}
declare const c: C;
c.m(1);
