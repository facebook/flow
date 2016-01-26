/* @flow */

class A {
  prop: string;
  method(): string {
    return "A";
  }
}

class B {
  test(): string {
    if (super.prop) {
      return super.prop;
    }
    return "B";
  }
}
