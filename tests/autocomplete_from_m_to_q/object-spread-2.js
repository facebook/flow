//@flow

type A = string
type B = string
type C = string

type Foo = {foo: A, bar: B};
declare const y : {...Foo, bar: C};
y.
//^
