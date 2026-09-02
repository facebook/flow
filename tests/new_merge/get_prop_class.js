// @flow

declare class C {
  static s: number;
}

export const x = C.s;

class P<T> {
  static m(): void {}

  static usesThis(): typeof P {
    return this;
  }
}

export const y = P.m;
export const z = P.usesThis;
