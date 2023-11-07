// @flow

declare enum E {
  A,
  B,
}

E.A as E; // OK
E.A as empty; // ERROR

declare export enum F {
  N,
  M,
}

F.N as F; // OK
F.N as empty; // ERROR
