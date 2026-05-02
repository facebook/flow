declare class Foo {
  foo: string;
}

function foo0(x: ?string): string {
  return x && x || "";
}

function foo1(x: ?Foo): string {
  return x && x.foo || "";
}

function foo2(x: ?Class<Foo>): string {
  return x && new x().foo || "";
}

declare var TDR: {-current: {offsetWidth: number, ...}, ...};
if (TDR.current) { }

function testNotExistsClass() {
  declare class C {}
  declare class P<X> {}

  if (!C) C as empty; // okay
  if (!P) P as empty; // okay
}
