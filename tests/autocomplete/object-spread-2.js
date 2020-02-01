//@flow

type A = string
type B = string
type C = string

type Foo = {| foo: A, bar: B |};
declare var y : {| ...Foo, bar: C |};
y.  // <-- AUTOCOMPLETE REQUEST HERE
