import type Immutable from 'immutable';

type GenericFn = (input: string) => number;

export type Spec<T> = {
  foo: Map<string, number>,
  bar: string,
  baz: ?Immutable.List<string>,
  qux: T,
  fn: ?GenericFn,
  ...
};
