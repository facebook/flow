// @flow

declare const x: mixed;

const e = match (x) {
  [const a]: a,
  {foo: const a}: a,
  {const a}: a,
  1 as const a: a,
  2 as a: a,
  const a: a,
};
