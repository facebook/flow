// TypeScript enum members must be number or string literals, and a defaulted
// member can only be auto-numbered when it follows a numeric constant. Flow's
// parser accepts these shapes, but they are errors in TypeScript, so we report
// them rather than silently typing an invalid enum.

// Boolean initializer: tsc TS18033.
enum Bool {
  A = true, // ERROR: must be a number or string literal
}

// BigInt initializer: tsc TS18033.
enum Big {
  A = 1n, // ERROR: must be a number or string literal
}

// A defaulted member after a string member cannot be auto-numbered: tsc TS1061.
enum Mixed {
  A = "a",
  B, // ERROR: must have initializer (previous member is not numeric)
}

// The errored member is degraded to a computed value (typed as its number
// representation), so it is not silently typed as the stale ordinal `0`.
Mixed.B satisfies number; // OK
Mixed.B satisfies 0; // ERROR: B is computed, not the literal `0`

// A defaulted member IS allowed when the immediately preceding member is numeric,
// even if an earlier member was a string. tsc accepts this.
enum Recover {
  A = "a",
  B = 2,
  C, // 3 (auto-numbered from the preceding numeric member)
}
Recover.C satisfies 3; // OK

// A numeric member name is rejected by TypeScript (TS2452: "An enum member cannot
// have a numeric name"), even though Flow's parser accepts a string-literal
// member name. Matching tsc, non-canonical forms (`"1e3"`, `"01"`) and
// `NaN`/`Infinity` are NOT numeric names, so only `"1"` errors below.
enum Numeric {
  "1" = 5, // ERROR: numeric name
  foo = 6, // ok: identifier name
  "1e3" = 7, // ok: not a canonical numeric name
}
// String-named members are otherwise usable as values.
Numeric.foo satisfies 6; // OK
