// @flow

enum E {
  A,
  B,
  C,
}

declare var e: E;

// flowlint-next-line require-explicit-enum-switch-cases:error
switch (e) { // OK
  case E.A: break;
  case E.B: break;
  case E.C: break;
}

// flowlint-next-line require-explicit-enum-switch-cases:error
switch (e) { // Error
  case E.A: break;
  default: break;
}
