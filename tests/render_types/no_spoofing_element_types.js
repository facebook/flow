function Foo() { return null }
declare const x: {+type: typeof Foo};
(x: $Renders<React$Element<typeof Foo>>); // ERROR!

component Bar() { return null }
declare const y: {+type: Bar};
(y: $Renders<Bar>); // ERROR! TODO better error message
