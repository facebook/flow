//@flow
import * as React from 'react';

function Foo(): React.Node {}

component Bar() renders React.Element<typeof Foo> { // invalid-render
  return <Foo />; // OK
}

(<Bar />) as renders React.Element<typeof Foo>; // type checks, but invalid-render

function Baz(): renders React.Element<typeof Bar> {
  return <Bar />;
} // OK
component Qux() {
  return null;
}
(<Baz />) as renders React.Element<typeof Bar>; // OK
(<Baz />) as renders React.Element<typeof Qux>; // ERROR

function RendersBaz(): renders React.Element<typeof Baz> { // invalid-render
  return <Baz />;
}
(<RendersBaz />) as renders Bar; // OK
(<RendersBaz />) as renders Qux; // ERROR

function LongerChain(): renders React.Element<typeof RendersBaz> { // invalid-render
  return <RendersBaz />;
}
(<LongerChain />) as renders Bar; // OK
(<LongerChain />) as renders Qux; // ERROR
