// An empty enum has no members, so Flow models its bare type as `empty` (the
// union of zero member literals): nothing is assignable to it.
enum E {}

// DEVIATION FROM TS (Flow is stricter): tsc does not model an empty enum as
// `never`/`empty` — it accepts `const bad: E = 0` with no error. Flow rejects it,
// since the empty member union admits nothing.
const bad: E = 0; // ERROR

// The enum value itself is still a usable (empty) object.
declare function takesObj(o: { readonly [string]: unknown }): void;
takesObj(E); // OK
