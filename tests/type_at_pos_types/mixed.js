// @flow

type A = {
  kind: 'A',
  metadata: {[key: string]: unknown},
 ...};

type B = {
  kind: 'B',
  metadata: {[key: string]: unknown},
 ...};

type AB = A | B;

const foo: Array<AB> = [];

foo.forEach(ab => {
  const local = ab;
//              ^?
});
