// @flow

type Checks = {assertTruthy: (x: unknown) => asserts x};

function shadowedType() {
  type Checks = {assertTruthy: (x: unknown) => void};
  function check(checks: Checks, x: ?string): number {
    checks.assertTruthy(x);
    return x.length; // ERROR
  }
  return check({assertTruthy: (x: unknown): void => {}}, null);
}

function localInterface() {
  interface Checks {
    assertTruthy(x: unknown): void;
  }
  function check(checks: Checks, x: ?string): number {
    checks.assertTruthy(x);
    return x.length; // ERROR
  }
}

function shadowedTypeParameter<Checks extends {assertTruthy: (x: unknown) => void}>(
  checks: Checks,
  x: ?string,
): number {
  checks.assertTruthy(x);
  return x.length; // ERROR
}
