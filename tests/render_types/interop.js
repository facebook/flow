//@flow
import * as React from 'react';

function Foo(): React.Node {}

component Bar() renders React.Element<typeof Foo> {
  return <Foo />; // OK
}

(<Bar />: renders React.Element<typeof Foo>); // OK

function Baz(): renders React.Element<Bar> { return <Bar />; } // OK
component Qux() { return null; }
(<Baz />: renders Bar); // OK
(<Baz />: renders Qux); // ERROR

function RendersBaz(): renders React.Element<typeof Baz> {
  return <Baz />;
}
(<RendersBaz />: renders Bar); // OK
(<RendersBaz />: renders Qux); // ERROR

function LongerChain(): renders React.Element<typeof RendersBaz> { return <RendersBaz />; }
(<LongerChain />: renders Bar); // OK
(<LongerChain />: renders Qux); // ERROR
