import {ExportedAssertions} from './exports';

type Checks = {
  assertString: (x: unknown) => asserts x is string,
  assertTruthy: (x: unknown) => asserts x,
};

class Base {
  assertString(x: unknown): asserts x is string {
    if (typeof x !== 'string') throw new Error();
  }

  assertTruthy(x: unknown): asserts x {
    if (!x) throw new Error();
  }

  static assertString(x: unknown): asserts x is string {
    if (typeof x !== 'string') throw new Error();
  }
}

class Derived extends Base {
  checks: Checks;

  viaThis(x: unknown) {
    this.assertString(x);
    x as string;
    x as number; // ERROR
  }

  viaSuper(x: unknown) {
    super.assertString(x);
    x as string;
    x as number; // ERROR
  }

  viaProperty(x: unknown) {
    this.checks.assertString(x);
    x as string;
    x as number; // ERROR
  }

  viaArrow(x: unknown): () => void {
    return () => {
      super.assertString(x);
      x as string;
      x as number; // ERROR
    };
  }

  bare(x: string | null) {
    this.assertTruthy(x);
    x as string;
    x as null; // ERROR
  }

  condition(x: unknown) {
    super.assertTruthy(typeof x === 'string');
    x as string;
    x as number; // ERROR
  }

  alwaysThrows(): empty {
    this.assertTruthy(false);
  }

  static viaThis(x: unknown) {
    this.assertString(x);
    x as string;
    x as number; // ERROR
  }

  static viaSuper(x: unknown) {
    super.assertString(x);
    x as string;
    x as number; // ERROR
  }

  field: (x: unknown) => void = (x: unknown) => {
    this.assertString(x);
    x as string;
    x as number; // ERROR
  };

  nestedFunction(): void {
    function differentThis(this: {assertString: (x: unknown) => void}, x: unknown) {
      this.assertString(x);
      x as string; // ERROR
    }
    const callback = function(this: Checks, x: unknown) {
      this.assertString(x);
      x as string;
      x as number; // ERROR
    };
  }
}

function explicitThis(this: Checks, x: unknown) {
  this.assertString(x);
  x as string;
  x as number; // ERROR
}

const explicitThisExpression = function(this: Checks, x: unknown) {
  this.assertTruthy(typeof x === 'string');
  x as string;
  x as number; // ERROR
};

class Generic<T> {
  assertValue(x: unknown): asserts x is T { throw new Error(); }

  check(x: unknown) {
    this.assertValue(x);
    x as T;
  }

  static assertString(x: unknown): asserts x is string {
    if (typeof x !== 'string') throw new Error();
  }

  static check(x: unknown) {
    this.assertString(x);
    x as string;
    x as number; // ERROR
  }
}

function localClass() {
  class Local extends Base {
    check(x: unknown) {
      this.assertString(x);
      x as string;
      x as number; // ERROR
    }
  }
  return Local;
}

const Anonymous = class extends Base {
  check(x: unknown) {
    super.assertString(x);
    x as string;
    x as number; // ERROR
  }
};

const Named = class NamedInner extends Base {
  check(x: unknown) {
    this.assertString(x);
    x as string;
    x as number; // ERROR
  }
};

class ImportedBase extends ExportedAssertions {
  static check(x: unknown) {
    super.assertNumber(x);
    x as number;
    x as string; // ERROR
  }
}
