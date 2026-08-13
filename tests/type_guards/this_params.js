// @flow

// Interaction between an explicit `this` parameter and a `this` type guard.
// An explicit `this` param moves the binding write for `this` onto the param,
// which the consistency check has to account for.

import {
  Sub as SubImported,
  WithThisParam as WithThisParamImported,
} from './this_class_exports';

function withThisParam() {
  class A {
    isB(this: A): this is B {
      return this instanceof B; // okay
    }

    isBUnchecked(this: A): this is B {
      return true; // error A ~> B
    }

    // Narrowing the `this` param below the enclosing class is rejected on its
    // own; it does not otherwise disturb the guard check.
    isBNarrowed(this: B): this is B { // error A ~> B on the `this` param
      return true;
    }

    // Delegating still works with an explicit `this` param.
    viaGuard(this: A): this is B {
      return this.isB(); // okay
    }

    withOtherParams(this: A, x: number, y: string): this is B {
      return this instanceof B; // okay
    }
  }

  class B extends A {}

  declare const a: A;
  if (a.isB()) {
    a as B; // okay
  } else {
    a as B; // error
  }
  if (a.withOtherParams(1, '')) {
    a as B; // okay
  }
}

function thisParamOnNonMethods() {
  class C {}

  // Not a class method, so still rejected even with a `this` param.
  function fn(this: C): this is C { // error only on class/interface methods
    return true;
  }

  const fnExpr = function (this: C): this is C { // error only on class/interface methods
    return true;
  };

  const obj = {
    m(this: C): this is C { // error only on class/interface methods
      return true;
    },
  };
}

function thisParamAndParamGuard() {
  class A {
    // A guard over a regular parameter in a method that also has a `this` param.
    isB(this: A, x: unknown): x is B {
      return x instanceof B; // okay
    }

    // `this` cannot be used as a regular parameter guard name.
    bad(this: A, x: unknown): this is B {
      return x instanceof B; // error twice: `this` is not refined by the body
    }
  }
  class B extends A {}

  declare const a: A;
  declare const x: unknown;
  if (a.isB(x)) {
    x as B; // okay
  }
}

function crossFile() {
  declare const w: WithThisParamImported;
  if (w.isSub()) {
    w as SubImported; // okay
  } else {
    w as SubImported; // error
  }
}
