declare class Foo {
  a?: string;
  b?: boolean;
  readonly c?: string;
  static d?: string;
}

declare const foo: Foo;
foo.a as string | void; // OK
foo.a as empty; // ERROR
foo.b as boolean | void; // OK
foo.c as string | void; // OK
Foo.d as string | void; // OK
