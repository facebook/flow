//@flow
import * as React from 'react';

function Foo(): React.Node {}

component Bar() renders React.Element<typeof Foo> { // invalid-render, turned into any
  return <Foo />; // OK
}

(<Bar />) as renders React.Element<typeof Foo>; // invalid-render, so RHS becomes any

function Baz(): renders React.Element<typeof Bar> {
  return <Bar />;
} // OK
component Qux() {
  return null;
}
(<Baz />) as renders React.Element<typeof Bar>; // OK
(<Baz />) as renders React.Element<typeof Qux>; // OK, since the super render of Bar is any due to invalid-render

function RendersBaz(): renders React.Element<typeof Baz> { // invalid-render
  return <Baz />;
}
(<RendersBaz />) as renders Bar; // OK
(<RendersBaz />) as renders Qux; // OK, since the super render of Baz, which is Bar, which has any super-render due to invalid-render

function LongerChain(): renders React.Element<typeof RendersBaz> { // invalid-render
  return <RendersBaz />;
}
(<LongerChain />) as renders Bar; // OK
(<LongerChain />) as renders Qux; // OK, since the super render of Bar is any due to invalid-render
