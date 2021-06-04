// @flow

enum U {
  A,
  B,
  C,
  ...
}

declare var u: U;

// flowlint-next-line require-explicit-enum-switch-cases:error
switch (u) { // OK
  case U.A: break;
  case U.B: break;
  case U.C: break;
  default: break;
}

// flowlint-next-line require-explicit-enum-switch-cases:error
switch (u) { // Error
  case U.A: break;
  default: break;
}
