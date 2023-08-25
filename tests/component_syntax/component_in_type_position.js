//@flow

component Foo() { return null }

const x: Foo = Foo; // OK!

component Bar() {return null}

const y: Foo = Bar; // ERROR! Bar ~> Foo

component Poly<T>() { return null }

type NumberPoly = Poly<number>;
type NoArgPoly = Poly; // ERROR!
