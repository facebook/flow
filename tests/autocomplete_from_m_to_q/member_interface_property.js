// @flow

interface Foo {
  bar: string,
  /** @deprecated */
  baz: string,
}

function f(x: Foo) {
  x.
//  ^
}
