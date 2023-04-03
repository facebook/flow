// @flow

class Foo {
  /**
   * foo
   * @param x a string
   */
  foo(x: string) {}

  /**
   * bar
   * @param y a number
   */
  static bar(y: number) {}
}

new Foo().foo(
  /* here */
);

Foo.bar(
  /* here */
)

const o = {
  /**
   * baz
   * @param z a bigint
   */
  baz: function(z: bigint) {}
}

o.baz(
  /* here */
);
