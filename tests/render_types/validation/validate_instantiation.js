type X = renders null; // ERROR: subtype of React$Node, but invalid-render
type Y = renders {}; // ERROR: not subtype of React$Node, invalid-render

component Foo() { return null }

type Z = renders ExactReactElement_DEPRECATED<typeof Foo>; // OK!
