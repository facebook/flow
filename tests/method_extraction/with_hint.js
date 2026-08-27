declare function expectMixed(x: unknown): void;
declare function expectAny(x: any): void;

class Foo {
  method(): number { return 3; }
}

const foo = new Foo();
expectMixed(foo.method); // OK
expectAny(foo.method); // OK

const method = foo.method;
method(); // ERROR - the preserved receiver is missing
