//@flow
import * as React from 'react';

function Foo(): React.Node {}

component Bar() renders React.Element<typeof Foo> {
  return <Foo />; // OK
}

(<Bar />: $Renders<React.Element<typeof Foo>>); // OK

function Baz(): $Renders<React.Element<Bar>> { return <Bar />; }
component Qux() { return null; }
(<Baz />: $Renders<Bar>); // OK
(<Baz />: $Renders<Qux>); // ERROR

function RendersBaz(): $Renders<React.Element<typeof Baz>> {
  return <Baz />;
}
(<RendersBaz />: $Renders<Bar>); // OK
(<RendersBaz />: $Renders<Qux>); // ERROR

function LongerChain(): $Renders<React.Element<typeof RendersBaz>> { return <RendersBaz />; }
(<LongerChain />: $Renders<Bar>); // OK
(<LongerChain />: $Renders<Qux>); // ERROR
