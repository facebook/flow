//@flow

type Foo = { a : number, b : string };
type Bar = Foo;
type Baz = Bar;
declare var x : Baz;
x.
