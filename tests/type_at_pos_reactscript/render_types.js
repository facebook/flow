type X = renders null;
//   ^
type Y = renders (null | number);
//   ^
component Foo() { return null }
component Bar() { return null }

type Z = renders Foo;
//   ^
type U = renders (React$Element<Foo> | React$Element<Bar>);
//   ^
type V = renders React$Element<Foo> | React$Element<Bar>;
//   ^
