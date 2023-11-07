/**
 * @format
 * @flow
 */

class X {}

const x1 = new X();
const x2: X = new X();
const x3 = x1;
const x4 = x2;
const x5: X = x1;
const x6 = x1;

x1 as empty; // X ~> empty. Ref should point to first `new X()`.
x2 as empty; // X ~> empty. Ref should point to first `X` annotation.
x3 as empty; // X ~> empty. Ref should point to first `new X()`.
x4 as empty; // X ~> empty. Ref should point to first `X` annotation.
x5 as empty; // X ~> empty. Ref should point to second `X` annotation.
x6 as empty; // X ~> empty. Ref should point to second `X` annotation.

const y1 = 42;
const y2: number = -42;
const y3 = y1;
const y4 = y2;
const y5: number = y1;
const y6 = y1;

y1 as empty; // number ~> empty. Ref should point to 42.
y2 as empty; // number ~> empty. Ref should point to first `number` annotation.
y3 as empty; // number ~> empty. Ref should point to 42.
y4 as empty; // number ~> empty. Ref should point to first `number` annotation.
y5 as empty; // number ~> empty. Ref should point to second `number` annotation.
y6 as empty; // number ~> empty. Ref should point to 42.

function fn1(x: string) {
  x as number; // string ~> number. Ref should point to `x: string`.
}

function fn2<T>(x: T) {
  x as number; // T ~> number. Ref should point to `x: T`.
}
