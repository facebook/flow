// @flow

class Foo {
  bar: number
  #baz: boolean
}

function callMeMaybe(foo: ?Foo) {
  return foo?.
//            ^
}
