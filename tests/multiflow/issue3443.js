// @flow

// Adapted from https://github.com/facebook/flow/issues/3443

class A {
    f(...args: any[]) {}
}

class B extends A {
    f(...args: any[]) {
      this.f(...args);
    }
}

function foo(...args: Array<number>) {
  foo(1, ...args);
}
foo(123);
