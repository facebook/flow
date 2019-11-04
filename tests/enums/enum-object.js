// @flow

enum E {
  A,
  B,
}

enum F {
  A,
  B,
}

// Comparison of enum object types
type EO = typeof E;
(E: EO); // Valid
(F: EO); // Error: types are incompatible

// Invalid access from enum object type
EO.A;
