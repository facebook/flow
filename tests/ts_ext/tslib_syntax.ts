declare class Foo {
  a?: string; // OK
}

declare const foo: Foo;
foo.a satisfies string | void; // OK
