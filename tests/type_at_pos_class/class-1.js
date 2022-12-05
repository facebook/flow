// @flow

class A {
  m() { }
}

class B extends A {
  m(): number { return 1; }
  constructor() {
    super();
  }
}
