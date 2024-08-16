function Foo() { return null }
declare const x: {+type: typeof Foo};
x as ExactReactElement_DEPRECATED<typeof Foo>; // ERROR!
x as renders ExactReactElement_DEPRECATED<typeof Foo>; // invalid-render makes RHS any

component Bar() { return null }
declare const y: {+type: typeof Bar};
y as renders Bar; // ERROR! TODO better error message
