// Numeric enums have a runtime reverse mapping: `Color[1] === "Red"`. TypeScript
// types the reverse lookup as `string` (not the literal member name).
enum Color {
  Red = 1,
  Green = 2,
}

// Reverse lookup by a member's numeric value is allowed and typed as `string`.
const name1 = Color[1]; // OK
name1 satisfies string; // OK
Color[2] satisfies string; // OK

// It is `string`, not the literal "Red", matching TS.
Color[1] satisfies "Red"; // ERROR

// Auto-numbered members get reverse mappings too.
enum Dir {
  Up, // 0
  Down, // 1
}
Dir[0] satisfies string; // OK
Dir[1] satisfies string; // OK

// String enums have no runtime reverse mapping.
enum Status {
  Active = "active",
}
Status["active"]; // ERROR: no reverse mapping for string enums
