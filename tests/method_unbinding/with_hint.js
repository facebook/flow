declare function expectMixed(x: mixed): void;
declare function expectAny(x: any): void;

class Foo {
  method(): number { return 3; }
}

const foo = new Foo();
expectMixed(foo.method); // OK
expectAny(foo.method); // OK
