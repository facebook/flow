//@flow
import * as React from 'react';

function Foo(): React.Node {}

component Bar() renders React.Element<typeof Foo> {
  return <Foo />; // OK
}

(<Bar />: renders React.Element<typeof Foo>); // OK

function Baz(): renders React.Element<typeof Bar> { return <Bar />; } // OK
component Qux() { return null; }
(<Baz />: renders React.Element<typeof Bar>); // OK
(<Baz />: renders React.Element<typeof Qux>); // ERROR

function RendersBaz(): renders React.Element<typeof Baz> {
  return <Baz />;
}
(<RendersBaz />: renders React.Element<typeof Bar>); // OK
(<RendersBaz />: renders React.Element<typeof Qux>); // ERROR

function LongerChain(): renders React.Element<typeof RendersBaz> { return <RendersBaz />; }
(<LongerChain />: renders React.Element<typeof Bar>); // OK
(<LongerChain />: renders React.Element<typeof Qux>); // ERROR
