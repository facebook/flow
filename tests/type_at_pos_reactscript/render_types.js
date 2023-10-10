type X = renders null;
//   ^
type Y = renders (null | number);
//   ^
component Foo() { return null }
component Bar() { return null }

type Z = renders Foo; 
//   ^
type U = renders (Foo | Bar);
//   ^
type V = renders Foo | Bar;
//   ^

type MaybeRenders = renders? Foo;
//   ^

type StarRenders = renders* Foo;
//   ^
