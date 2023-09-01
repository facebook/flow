function Foo() { return null }
declare const x: {+type: typeof Foo};
(x: renders React$Element<typeof Foo>); // ERROR!

component Bar() { return null }
declare const y: {+type: typeof Bar};
(y: renders React$Element<typeof Bar>); // ERROR! TODO better error message
