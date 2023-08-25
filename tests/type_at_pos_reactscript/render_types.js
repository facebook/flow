type X = $Renders<null>;
//   ^
type Y = $Renders<null | number>;
//   ^
component Foo() { return null }
component Bar() { return null }

type Z = $Renders<Foo>;
//   ^
type U = $Renders<Foo | Bar>;
//   ^
