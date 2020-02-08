//@flow

class A {
  fn1(x: ?string) {}

  fn2 = (x: ?number) => {};

  constructor(x: ?number) {
    this.fn3 = (y: number) => {};
  }

  static fn4(x: ?number) {}
}

function f(x: A) {}
