//@flow

type Foo = { a : number, b : string };
type Bar = Foo;
type Baz = Bar;
declare const x : Baz;
x.
//^
