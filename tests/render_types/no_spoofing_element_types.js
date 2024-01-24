function Foo() { return null }
declare const x: {+type: typeof Foo};
x as renders React$Element<typeof Foo>; // ERROR!

component Bar() { return null }
declare const y: {+type: typeof Bar};
y as renders Bar; // ERROR! TODO better error message
