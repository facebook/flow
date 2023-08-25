type X = $Renders<null>; // OK!
type Y = $Renders<{}>; // ERROR

component Foo() { return null }

type Z = $Renders<Foo>; // OK!
