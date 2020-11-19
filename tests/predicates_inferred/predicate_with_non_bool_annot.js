// @flow

type T = 'a' | 'b';

function validate_1(x: ?string): ?T %checks {
  return (x === 'a' || x === 'b') ? x : null;
}

function validate_2(x: ?string): ?T %checks {
  return (x === 'a' || x === 'c') ? x : null; // error: 'c' incompatible with T
}
