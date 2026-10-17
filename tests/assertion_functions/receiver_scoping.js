// @flow

class Base {
  assertTruthy(x: unknown): asserts x {
    if (!x) throw new Error();
  }
}

function shadowedSuperclass(): number {
  class Base {
    assertTruthy(x: unknown): void {}
  }
  class Derived extends Base {
    check(x: ?string): number {
      super.assertTruthy(x);
      return x.length; // ERROR
    }

    checkThis(x: ?string): number {
      this.assertTruthy(x);
      return x.length; // ERROR
    }
  }
  return new Derived().check(null);
}

class PlainBase {
  assertTruthy(x: unknown): void {}
}

function localSuperclass() {
  class PlainBase {
    assertTruthy(x: unknown): asserts x {
      if (!x) throw new Error();
    }
  }
  class Derived extends PlainBase {
    check(x: ?string): void {
      super.assertTruthy(x);
      x as string;
      x as null; // ERROR
    }
  }
}

type Checks = {assertTruthy: (x: unknown) => asserts x, ...};

function shadowedThisAnnotation() {
  type Checks = {assertTruthy: (x: unknown) => void, ...};
  function check(this: Checks, x: ?string): number {
    this.assertTruthy(x);
    return x.length; // ERROR
  }
  const obj = {assertTruthy: (x: unknown): void => {}, check};
  return obj.check(null);
}

function localThisAnnotation() {
  type LocalChecks = {assertString: (x: unknown) => asserts x is string};
  function check(this: LocalChecks, x: unknown): void {
    this.assertString(x);
    x as string;
    x as number; // ERROR
  }
}
