// Duplicate member name
enum E1 {
  A,
  A, // ERROR
}

// Inconsistent member values (number enum, mix of initialized and uninitialized)
enum E2 {
  A = 1,
  B, // ERROR
}

// Invalid member initializer (wrong type for explicit boolean enum)
enum E3 of boolean {
  A = "hello", // ERROR
}

// Invalid member initializer (wrong type for explicit number enum)
enum E4 of number {
  A = true, // ERROR
}

// Invalid member initializer (wrong type for explicit string enum)
enum E5 of string {
  A = 1, // ERROR
}

// Boolean member not initialized (explicit)
enum E6 of boolean {
  A, // ERROR
}

// Boolean member not initialized (implicit)
enum E7 {
  A = true,
  B, // ERROR
}

// Number member not initialized (explicit)
enum E8 of number {
  A, // ERROR
}

// Number member not initialized (implicit)
enum E9 {
  A = 1,
  B, // ERROR
}

// BigInt member not initialized (explicit)
enum E10 of bigint {
  A, // ERROR
}

// BigInt member not initialized (implicit)
enum E11 {
  A = 1n,
  B, // ERROR
}

// String member inconsistently initialized (some with, some without)
enum E12 of string {
  A = "a",
  B, // ERROR
}

// String member inconsistently initialized (implicit)
enum E13 {
  A = "a",
  B, // ERROR
}

// Inconsistent member values (mixed literal types in implicit enum)
enum E14 { // ERROR
  A = true,
  B = 1,
}

// Invalid member initializer for symbol enum (symbol members cannot have initializers)
enum E15 of symbol {
  A = "hello", // ERROR
}

// String literal member name (not allowed)
enum E16 of string {
  'MAIN' = "MAIN", // ERROR
  'NOPREVIEW' = "NOPREVIEW", // ERROR
  Good = "Good",
}

// Defaulted string literal member name (not allowed)
enum E17 {
  'FOO', // ERROR
  Bar,
}

// String literal member name with special characters (not allowed)
enum E18 of string {
  'foo-bar' = "foo-bar", // ERROR
  'has space' = "has space", // ERROR
}
