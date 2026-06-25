// A `declare enum` is ambient even in a (non-.d.ts) .ts file, so its uninitialized
// members are computed rather than auto-numbered literals (matches TS).
declare enum E {
  A,
  B,
}

// Computed members are typed as their representation (number), so the bare enum
// type is permissive rather than a false `0 | 1` union.
const e: E = 5; // OK
E.A satisfies number; // OK
E.A satisfies 0; // ERROR: A is computed, not the literal `0`

// An explicit initializer is still a known literal even in a `declare enum`.
declare enum F {
  A = 1,
  B, // computed (no auto-numbered literal in an ambient enum)
}
F.A satisfies 1; // OK
F.B satisfies number; // OK
F.B satisfies 2; // ERROR: B is computed in an ambient enum

// In an ambient enum a defaulted member after a string member is allowed (it is
// computed) — unlike a non-ambient enum, where it would be a TS1061 error. tsc
// accepts this with no error.
declare enum G {
  A = "a",
  B, // computed; no error because the enum is ambient
}
G.B satisfies number; // OK
