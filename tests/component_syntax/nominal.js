//@flow

import * as React from 'react';

component Foo() {
  return null;
}

component Bar() {
  return null;
}

(Bar: typeof Foo); // ERROR
(Foo: typeof Bar); // ERROR
(Foo: typeof Foo); // OK
(Foo: React.AbstractComponent<{}, mixed, React.Node>); // OK
(Foo: React.AbstractComponent<{}, mixed, null>); // ERROR, default render is React.Node
(Foo: React.AbstractComponent<{foo: number}, mixed, React.Node>); // ERROR

component Baz(foo: number) renders null {
  return null;
}

(Baz: React.AbstractComponent<{foo: number}, mixed, $Renders<Baz>>); // OK
(Baz: React.AbstractComponent<{foo: number}, mixed, $Renders<null>>); // OK
