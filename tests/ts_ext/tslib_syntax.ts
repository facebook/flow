declare class Foo {
  a?: string; // OK
}

declare const foo: Foo;
foo.a as string | void; // OK
