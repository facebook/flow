// @flow

class Bounds<N extends number, F extends () => N> {
  foo: F;
  bar() {
    this.foo().
//             ^
  }
}
