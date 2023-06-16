component Foo(...x: number) { return null; } // ERROR, not an object
component Bar(...x?: number) {return null; } // ERROR optional
component Baz(...x?: {foo: number}) {return null; } // ERROR optional
