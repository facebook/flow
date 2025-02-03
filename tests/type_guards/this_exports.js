declare export class A {
  isB(): this is B;
  isB1(x: number): this is B;
  isB1Impl(x: number): implies this is B;
  isD(): this is D;
  static isB_static(): this is B; // error should not export this guard
}

declare export class B extends A {}
declare export class C extends B {}
declare export class D {}

export class E {
  isF(): this is F { // error
    return true;
  }
}

export class F extends E {}

export interface I {
  isB(): this is B;
}
