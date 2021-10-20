// @flow

declare class C {
  static s: number;
}

export const x = C.s;

class P<T> {
  static m(): void {}
}

export const y = P.m;
