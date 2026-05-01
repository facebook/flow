component Foo() { return null }
component Bar() { return null }
component Baz() { return null }

declare const a: renders Baz;
a as renders ExactReactElement_DEPRECATED<typeof Foo | typeof Bar>; // ERROR

declare const b: renders (Foo | Bar);
b as renders ExactReactElement_DEPRECATED<typeof Foo | typeof Bar>; // OK

declare const c: renders Foo;
c as renders ExactReactElement_DEPRECATED<typeof Foo | typeof Bar>; // OK
