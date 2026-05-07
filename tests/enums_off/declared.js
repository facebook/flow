declare enum E { // ERROR: enums are off
  A,
  B,
}

declare const e: E; // ERROR: can't resolve name `E`
