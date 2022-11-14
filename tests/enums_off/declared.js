// @flow

declare enum E { // ERROR: enums are off
  A,
  B,
}

declare var e: E; // ERROR: can't resolve name `E`
