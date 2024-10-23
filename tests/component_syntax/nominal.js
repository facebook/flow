//@flow

import * as React from 'react';

component Foo() {
  return null;
}

component Bar() {
  return null;
}

Bar as typeof Foo; // ERROR
Foo as typeof Bar; // ERROR
Foo as typeof Foo; // OK
Foo as React.ComponentType<{}>; // OK
Foo as React.AbstractComponent<{}, mixed, null>; // ERROR, default render is React.Node
Foo as React.ComponentType<{foo: number}>; // ERROR

component Baz(foo: number) renders null { // invalid-render
  return null;
}

Baz as component(foo: number) renders Baz; // OK
Baz as React.AbstractComponent<{foo: number}, mixed, renders null>; // OK
