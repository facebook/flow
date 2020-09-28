// @flow

class Foo {
  bar: Bar
}

class Bar {
  baz: number
  #qux: string
}

function callMeMaybe(foo: ?Foo) {
  return foo?.bar.
//                ^
}
