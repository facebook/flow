// @flow

type Alias = {
  foo(): void,
  bar: string,
};

const a: Alias = { foo() {}, bar: ''};
a.foo();
a.foo();
a.bar;
a.bar;

const b = {bar: ''};
b.bar;
b.bar;

type Spreaded = {
  ...Alias,
};

const f: Spreaded = { foo() {}, bar: '', baz: 5 };
if (f.foo != null) { f.foo() };

type Alias2 = {
  foo(): void,
  bar: string,
};

const g = { foo() {}, bar: ''};
(g: Alias);
(g: Alias2);

function destructuring(x: Alias): void {
  const {foo, bar: baz} = x;
}
