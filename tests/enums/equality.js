// @flow

enum E {
  A,
  B,
}

enum F {
  A,
  B,
}

const e = E.A;
const eo: typeof E = E;
const maybeE: ?E = null;
const ef: E | F = E.A;

const s: string = "hi";

// Comparison of enums with == or != is banned
s == e; // Error
e == s; // Error

s != e; // Error
e != s; // Error

e == E.A; // Error
ef == E.A; // Error
F.A == E.A; // Error

eo == s; // Error

// Except comparison of enum to null or void, it is allowed
maybeE != null; // OK
maybeE != undefined; // OK
