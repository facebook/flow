/* @flow */

class A {}
class B extends A {}
class C extends B {}

declare const a: A;
declare const b: B;
declare const c: C;

class Base {
  x: B;
  readonly pos: B;
  writeonly neg: B;
  get get(): B { return this.x };
  set set(value: B): void { this.x = value };
  get getset(): B { return this.x };
  set getset(value: B): void { this.x = value };
}

export {A, B, C, Base}
