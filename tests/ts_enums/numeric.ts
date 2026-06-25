// Numeric TS enum: explicit + auto-incremented members.
enum Color {
  Red = 1,
  Green, // 2
  Blue, // 3
}

// Each member has a singleton literal type.
Color.Red satisfies 1; // OK
Color.Green satisfies 2; // OK
Color.Blue satisfies 3; // OK

// Members are assignable to their representation type.
Color.Red satisfies number; // OK

// The bare enum name is a type = union of member literals.
const a: Color = Color.Red; // OK
const b: Color = 1; // OK: 1 is a member value

// A literal that is not a member value is an error.
const c: Color = 99; // ERROR

// Auto-numbering starts at 0 when the first member is defaulted.
enum Dir {
  Up, // 0
  Down, // 1
}
Dir.Up satisfies 0; // OK
Dir.Down satisfies 1; // OK
