declare class A {}

type AOrString = A | string;

declare const C: Class<string>;
class B extends C {}

function invariant(x: unknown) {}

function foo(value: AOrString) {
  invariant(value instanceof B);
}

function more_tests() {
  class A  { }
  class B extends A { }
  class C { }

  declare const invariant: any;

  function f(a: A, b: B, c: C) {
    invariant(a instanceof B);
    invariant(b instanceof A);
    invariant(c instanceof A);
    invariant(a instanceof C)
  }
}

declare const obj: {};
class D extends obj {}
