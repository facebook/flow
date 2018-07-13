// @flow

function f(x: number): void {}

const a: number = 1;
const b: number = 2;
const c: any = 3;
f(a+b);
f(b+c);

try {
  f(1);
} catch (e) {
  f(1);
}
