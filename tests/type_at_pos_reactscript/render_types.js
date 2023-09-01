type X = renders null;
//   ^
type Y = renders (null | number);
//   ^
component Foo() { return null }
component Bar() { return null }

type Z = renders React$Element<typeof Foo>;
//   ^
type U = renders (React$Element<typeof Foo> | React$Element<typeof Bar>);
//   ^
type V = renders React$Element<typeof Foo> | React$Element<typeof Bar>;
//   ^
