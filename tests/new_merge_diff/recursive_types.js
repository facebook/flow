// @flow

export const x: x = 0; // error recursive

export const y: typeof y.f = 0; // error recursive

export const z: typeof z = 1; // TODO error recursive

// $FlowExpectedError[incompatible-use]
export type T = T.A | T.B;

export type S = string | R; // okay
export type R = { p: S };

declare export class C {
  static R: { ...typeof C.R }; // TODO error
}
