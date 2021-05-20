// @flow

class A {
  m : () => A = () => {
    return this;
  }
}

function foo(x) {}
foo(new A().m);
