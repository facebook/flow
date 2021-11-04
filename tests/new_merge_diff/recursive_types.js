// @flow

export const x: x = 0; // error recursive

export const y: typeof y.f = 0; // error recursive

export const z: typeof z = 1; // TODO error recursive

export function f(): typeof f.g { return 1; } // error recursive
f.g = 1;

// $FlowExpectedError[incompatible-use]
export type T = T.A | T.B;

export type S = string | R; // okay
export type R = { p: S };
