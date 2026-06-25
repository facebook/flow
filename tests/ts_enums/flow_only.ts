// Flow Enums support syntax that TypeScript enums do not. Because Flow forces
// enum parsing on for .ts/.d.ts files, these shapes parse successfully, so Flow
// reports them as not valid TypeScript rather than silently accepting them.

// Unknown members (`...`) are a Flow Enums feature; tsc has no equivalent.
enum WithUnknown {
  A = 1,
  ... // ERROR: unknown members not allowed in a TS enum
}

// An explicit representation type (`of T`) is a Flow Enums feature; a TS enum's
// representation is inferred from its members.
enum OfString of string { // ERROR: explicit representation type not allowed in a TS enum
  A = "a",
}
