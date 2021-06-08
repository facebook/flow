// @flow

type U =
  | A
  | B;


class A {
  foo() {}
}

class B {
  foo() {}
}


class C<T: U> {
  contents: T;

  m() {
    this.contents.foo(); // should not error
  }
}
