//@flow

component Foo() { return null }

const x: Foo = Foo; // OK!

component Bar() {return null}

const y: Foo = Bar; // ERROR! Bar ~> Foo
