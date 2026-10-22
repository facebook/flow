// A member used as a type denotes just that member's literal.
enum Color {
  Red = 1,
  Green = 2,
}

let r: Color.Red = Color.Red; // OK
r satisfies 1; // OK

// Assigning a different member is an error.
let r2: Color.Red = Color.Green; // ERROR

// Type keywords can name enum members used as generic type arguments.
enum Keyword {
  number = 1,
  string = 2,
  class = 3,
}

type BaseElement<T extends Keyword> = { type: T };
const numberElement: BaseElement<Keyword.number> = { type: Keyword.number }; // OK
const wrongNumberElement: BaseElement<Keyword.number> = { type: Keyword.string }; // ERROR

// Reserved words can also name enum member types.
const classMember: Keyword.class = Keyword.class; // OK
const wrongClassMember: Keyword.class = Keyword.number; // ERROR
