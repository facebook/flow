// A member used as a type denotes just that member's literal.
enum Color {
  Red = 1,
  Green = 2,
}

let r: Color.Red = Color.Red; // OK
r satisfies 1; // OK

// Assigning a different member is an error.
let r2: Color.Red = Color.Green; // ERROR
