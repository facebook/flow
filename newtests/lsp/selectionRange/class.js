// @flow

class A {}

interface I {}

class C extends A implements I {
  foo(): void {

  }

  get bar() {

  }

  baz<T>(): void {

  }

  propA: string;

  +propB: string = "foo";
}
