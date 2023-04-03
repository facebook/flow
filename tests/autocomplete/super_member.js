// @flow

class Foo {
  foo() {}
}

class Bar extends Foo {
  baz() {
    super.
//        ^
  }
}
