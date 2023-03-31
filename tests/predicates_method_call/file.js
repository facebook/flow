// @flow

{
  type A = 'A';
  type B = 'B';
  type T = A | B;

  function isA(value: T): %checks {
    return value === 'A';
  }

  function isB(value: T): %checks {
    return value === 'B';
  }

  const Predicates = { isA, isB };

  const x: T = 'A';

  if (isA(x)) {
    (x: A); // okay
    (x: B); // error
  }

  if (Predicates.isA(x)) {
    (x: A);  // okay
    (x: B);  // error
  }
}

import * as UtilsA from './utilsA';

function test1(id: ?string): string {
  if (!UtilsA.isValid(id)) {
    return '';
  }
  return id; // okay
}

import * as UtilsB from './utilsB';

function test2(id: ?string): string {
  if (!UtilsB.isValid(id)) {
    return '';
  }
  return id; // okay
}
