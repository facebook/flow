// @flow

type Alias = {
  foo(): void,
  bar: string,
}

const x: Alias = { foo() {}, bar: ''};
x.foo();
x.foo();
x.bar;
x.bar;
