// @flow

// `this` type guards on non-static methods of regular classes, focusing on the
// consistency check of the method body. See also this.js (declare classes and
// interfaces) and this_generics.js / this_params.js.

import {Base as BaseImported, Derived as DerivedImported} from './this_class_exports';
import type {Opaque as OpaqueImported} from './this_class_exports';

function subtyping() {
  class A {
    isB(): this is B {
      return this instanceof B; // okay
    }

    isBUnchecked(): this is B {
      return true; // error A ~> B
    }

    // The guard type does not have to be a subclass of the enclosing class. The
    // refinement then produces the intersection `A & Unrelated`.
    isUnrelated(): this is Unrelated {
      return this instanceof Unrelated; // okay
    }

    // Widening to a supertype is vacuously true.
    isA(): this is A {
      return true; // okay
    }

    isEmpty(): this is empty {
      return true; // error A ~> empty
    }

    isUnion(): this is B | C {
      return this instanceof B || this instanceof C; // okay
    }

    isIntersection(): this is B & Unrelated {
      return this instanceof B; // error twice: A & B ~> Unrelated, and negation
    }
  }

  class B extends A {}
  class C extends A {}
  class Unrelated {}

  declare const a: A;
  if (a.isB()) {
    a as B; // okay
  } else {
    a as A; // okay
    a as B; // error
  }

  // The guard is inherited, and refines the subclass to `B & C`.
  declare const c: C;
  if (c.isB()) {
    c as B; // okay
    c as C; // okay
  }

  // Refining a value that already has the guard type refines the else branch away.
  declare const b: B;
  if (b.isB()) {
    b as B; // okay
  } else {
    b as empty; // okay
  }

  declare const u: A;
  if (u.isUnrelated()) {
    u as A; // okay
    u as Unrelated; // okay
  }
}

function overriding() {
  class A {
    isB(): this is B {
      return this instanceof B;
    }
  }
  class B extends A {}
  class C extends A {}

  // Methods are invariant, so the guard type cannot change in an override.
  class Bad extends A {
    isB(): this is C { // error type guard is in invariant position
      return this instanceof C;
    }
  }

  // A one-sided guard cannot override a checked one either.
  class BadImplies extends A {
    isB(): implies this is C { // error twice: one-sided vs default guard, and C ~> B
      return this instanceof C;
    }
  }

  // An override may not drop the guard.
  class Dropped extends A {
    isB(): boolean { // error missing type guard
      return true;
    }
  }

  // Re-declaring the same guard is fine.
  class Same extends A {
    isB(): this is B {
      return this instanceof B; // okay
    }
  }
}

function delegation() {
  class A {
    isB(): this is B {
      return this instanceof B; // okay
    }

    extra(): boolean {
      return true;
    }

    // Delegating to another `this` type guard is the motivating case: no
    // suppression needed and the body is still checked.
    viaGuard(): this is B {
      return this.isB(); // okay
    }

    viaGuardNegated(): this is B {
      return !this.isB(); // error twice: positive refines to A, negation to B
    }

    // A conjunction with a non-refining operand does not refine the negation.
    viaGuardAndMore(): this is B {
      return this.isB() && this.extra(); // error negation does not refine away B
    }

    viaGuardAndMoreImplies(): implies this is B {
      return this.isB() && this.extra(); // okay one-sided
    }

    viaGuardOr(): this is B {
      return this.isB() || this.extra(); // error the `extra` branch is not refined
    }

    // TODO consistency checking does not look through a `const` binding. This
    // is a pre-existing limitation shared with guards over regular parameters,
    // not something specific to `this`.
    viaConst(): this is B {
      const r = this.isB();
      return r; // error twice: positive and negative
    }

    recursive(): this is B {
      return this.recursive(); // okay
    }
  }

  class B extends A {
    viaSuper(): this is B {
      return super.isB(); // error TODO super calls do not refine `this`
    }
  }
}

function havoc() {
  // A type guard variable that a closure reassigns is havoced, which makes the
  // guard unusable. `this` can never be reassigned, so that branch is
  // unreachable for a `this` guard -- and a refinement of `this` survives an
  // intervening call. The parameter case runs the same machinery in the same
  // class, so the `this` results are not passing vacuously.
  declare function sideEffect(): void;

  class A {
    paramIsHavoced(x: unknown): x is B { // error 'x' is havoced
      const write = () => {
        x = 1;
      };
      write();
      return x instanceof B;
    }

    thisAfterCall(): this is B {
      sideEffect();
      return this instanceof B; // okay
    }

    // The refinement is established before the call and read at the return.
    thisRefinementSurvivesCall(): this is B {
      if (!(this instanceof B)) {
        return false;
      }
      sideEffect();
      return true; // okay `this` is still refined to B
    }

    // A closure writing to an unrelated local does not disturb `this` either.
    thisSurvivesUnrelatedWrite(): this is B {
      let y = 0;
      const write = () => {
        y = 1;
      };
      if (!(this instanceof B)) {
        return false;
      }
      write();
      return true; // okay
    }
  }
  class B extends A {}
}

function returnPositions() {
  class A {
    // `return true` skips the negative check, `return false` skips the positive one.
    alwaysTrue(): this is B {
      return true; // error A ~> B
    }

    alwaysFalse(): this is B {
      return false; // error negation does not refine away B
    }

    alwaysFalseImplies(): implies this is B {
      return false; // okay one-sided
    }

    // Every return is checked independently.
    multipleReturns(x: boolean): this is B {
      if (x) {
        return this instanceof B; // okay
      }
      return true; // error A ~> B
    }

    // A method that never returns is not checked at all (same as parameter guards).
    neverReturns(): this is B {
      throw new Error('unreachable'); // okay
    }

    // A refinement inside a nested arrow does not escape it.
    insideArrow(): this is B {
      return [1].every(() => this instanceof B); // error twice: positive and negative
    }
  }
  class B extends A {}
}

function opaqueTypes() {
  // The requester's shape: an opaque type used as the guard type. Inside the
  // defining file the opaque type is transparent, so the check succeeds.
  class Wid {
    isLid(): this is Lid {
      return this instanceof LidImpl; // okay
    }

    isLidUnchecked(): this is Lid {
      return true; // error Wid ~> Lid
    }
  }
  class LidImpl extends Wid {}
  opaque type Lid: Wid = LidImpl;

  declare const w: Wid;
  if (w.isLid()) {
    w as Lid; // okay
  }
}

function structural() {
  class A {
    isB(): this is B {
      return this instanceof B;
    }
  }
  class B extends A {}

  class NoGuard {
    isB(): boolean {
      return true;
    }
  }

  declare function want(x: interface {isB(): this is B}): void;

  want(new A()); // okay
  want(new B()); // okay
  want(new NoGuard()); // error no type-guard

  interface I {
    isB(): this is B;
  }

  // An implementation of an interface method carrying a guard must repeat it.
  class Impl implements I {
    isB(): this is B {
      return this instanceof B; // okay
    }
  }

  class BadImpl implements I {
    isB(): boolean { // error missing type guard
      return true;
    }
  }

  function unbind(x: A) {
    const isB = x.isB; // error method-unbinding
    isB as () => boolean; // okay (isB still shows as a type guard)
  }
}

function objectGuardTypes() {
  // Object types as the guard type. Flow class instances are not subtypes of
  // object types, so the consistency check rejects every object form even when
  // the class plainly has the properties. This is a real difference from
  // TypeScript, where structural typing makes `this is {a: number}` work. The
  // Flow equivalent is an `interface` guard type, which is accepted -- see
  // `isIface` below, and the hint Flow attaches to the error.
  class A {
    a: number = 1;

    // Exact by default, and a class instance is inexact.
    isExact(): this is {a: number} {
      return typeof this.a === 'number'; // error twice: exactness, and negation
    }

    // Inexact does not help: the nominal/structural boundary is the problem.
    isInexact(): this is {a: number, ...} {
      return typeof this.a === 'number'; // error twice: class-object, and negation
    }

    // Not even the empty object type.
    isAnyObj(): this is {...} {
      return true; // error class instances are not subtypes of object types
    }

    // One-sided drops the negation obligation but not the incompatibility.
    isExactImplies(): implies this is {a: number} {
      return typeof this.a === 'number'; // error exactness only
    }

    // An object type the class does not satisfy fails twice over.
    isOther(): this is {zzz: string, ...} {
      return true; // error twice: class-object, and `zzz` missing
    }

    // The supported spelling.
    isIface(): this is interface {a: number} {
      return true; // okay
    }
  }

  declare const a: A;
  if (a.isIface()) {
    a.a as number; // okay
    a as interface {a: number}; // okay
  }

  // Not specific to `this`: a guard over a class-typed parameter is rejected the
  // same way, so this is the class/object boundary rather than anything about
  // `this` guards.
  declare function paramGuard(x: A): x is {a: number, ...}; // error class instances are not subtypes of object types
}

function classFields() {
  // A class field is not a method, whatever its initializer is.
  class A {
    isBArrow = (): this is B => { // TODO error, but an arrow closes over the instance `this`
      return this instanceof B;
    };

    // A function expression binds its own `this`, so this one is correctly rejected.
    isBFunction = function (): this is B { // error only on class/interface methods
      return true;
    };
  }
  class B extends A {}
}

function crossFile() {
  // The guard is recorded in the signature of a regular class, so it still
  // refines in other files.
  declare const b: BaseImported;
  if (b.isDerived()) {
    b as DerivedImported; // okay
  } else {
    b as DerivedImported; // error
  }

  // Outside its defining file the opaque type is abstract, so the refinement
  // produces the intersection rather than the underlying type.
  if (b.isOpaque()) {
    b as OpaqueImported; // okay
  }
}
