component Foo() { return null }
component Bar() { return null }
component Baz() { return null }

declare const a: renders Baz;
(a: renders React$Element<typeof Foo | typeof Bar>); // ERROR

declare const b: renders (Foo | Bar);
(b: renders React$Element<typeof Foo | typeof Bar>); // OK

declare const c: renders Foo;
(c: renders React$Element<typeof Foo | typeof Bar>); // OK
