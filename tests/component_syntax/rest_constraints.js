component Foo(...x: number) { return null; } // ERROR, not an object
component Bar(...x?: number) {return null; } // ERROR optional
component Baz(...x?: {foo: number}) {return null; } // ERROR optional

component DestructuredObj(
  ...{foo, bar, baz = false}: {foo: number, bar: number, baz?: number} // ERROR bad default for baz
) {
  (foo: number); // OK
  (bar: string); // ERROR
  return null;
}

component DestructuredArr(...[foo, bar]: {foo: number, bar: number}) { // ERROR destructure array
  return null;
}

component DestructuredArrWithArrAnnot(...[foo, bar]: [number, number]) { // ERROR destructure array, array not an object
  return null;
}

import * as React from 'react'
<DestructuredArr />; // ERROR because annot still provides a valid config type
<DestructuredArrWithArrAnnot />; // OK because previous not an object error causes config to be any
