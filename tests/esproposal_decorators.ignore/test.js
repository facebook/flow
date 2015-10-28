/* @flow */

@decorator1
class Foo {
  @decorator2
  method1() {}

  @decorator3
  @decorator4
  method2() {}
}
