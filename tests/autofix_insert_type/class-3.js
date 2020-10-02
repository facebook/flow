// @flow

class A {
  m(): this {
    return this;
  }
}

function foo(x) {}
foo(new A().m);
