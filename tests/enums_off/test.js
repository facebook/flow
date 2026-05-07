enum E { // Error: enums are off
  A,
  B,
}

declare const x: E; // Error: can't resolve name `E`
