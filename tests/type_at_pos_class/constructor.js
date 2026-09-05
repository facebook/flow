// @flow

class Foo {
  constructor(x: string) {

  }
}

const foo = new Foo("hi");

class Bar {
  constructor(x: number) {}
}

const ns = { Bar };

const bar = new ns.Bar(42);

const Anon = class {
  constructor(x: string) {}
};
