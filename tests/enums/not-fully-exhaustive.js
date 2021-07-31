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
