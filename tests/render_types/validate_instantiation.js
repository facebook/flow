type X = renders null; // OK!
type Y = renders {}; // ERROR

component Foo() { return null }

type Z = renders React$Element<typeof Foo>; // OK!
