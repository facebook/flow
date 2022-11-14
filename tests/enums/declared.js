// @flow

declare enum E {
  A,
  B,
}

(E.A: E); // OK
(E.A: empty); // ERROR
