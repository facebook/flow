// @flow
class Foo {
  static bar: string = "bar";
  baz: string;
  /** @deprecated */
  baz_DEPRECATED: number;
}

const foo = new Foo();
foo.
//  ^
