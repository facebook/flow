// @flow

class Foo {
  prop: ?string;
  fun(): void {}
}

const foo = new Foo();
foo.prop;
foo.fun();

if (foo.prop != null) {
  foo.prop;
}

function f(x: ?Foo): void {
  x.fun();
}

function f(x: Foo | null | void): void {
  x.fun();
}
