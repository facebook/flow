function Foo() { return null }
declare const x: {readonly type: typeof Foo};
x as ExactReactElement_DEPRECATED<typeof Foo>; // ERROR!

component Bar() { return null }
declare const y: {readonly type: typeof Bar};
y as renders Bar; // ERROR! TODO better error message
