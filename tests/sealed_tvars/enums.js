// @flow

enum A { Foo, Bar }
for (const x of A) {} // Error: not iterable, but there should be no enforcement errors

const a: A.Foo = A.Foo; // Error: enum-value-as-type

type Props = X
var y = X;

export enum X {
  AGE,
}
