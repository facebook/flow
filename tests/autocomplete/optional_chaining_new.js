// @flow

class Foo {
  bar: number
  #baz: bool
}

function callMeMaybe(foo: ?Foo) {
  return foo?.
//            ^
}
