// `typeof X` on an abstract class must preserve the abstract bit just like
// `Class<X>` does. Mirrors `basic.js` and `union_constructor.js` for the
// `typeof X` form, and `constructor_type.js` for the cross-form
// `typeof Abstract` -> `new () => T` rejection.

abstract class Animal {
  abstract makeSound(): string;
}

class Dog extends Animal {
  makeSound(): string {
    return "woof";
  }
}

// `typeof Animal` is the abstract constructor — calling `new` is rejected.
const ctor = Animal as typeof Animal; // OK
new ctor(); // ERROR

// `typeof Dog` is a concrete constructor — `new` is fine and the result is
// typed as `Dog`, so `.makeSound()` resolves.
const ctorOk = Dog as typeof Dog; // OK
new ctorOk().makeSound() as string; // OK

// `typeof Animal` does NOT flow into a non-abstract `new () => T` slot
// (cross-form analogue of `constructor_type.js:9`).
Animal as new () => Animal; // ERROR

// `typeof Animal` DOES flow into an `abstract new () => T` slot.
const c2 = Animal as abstract new () => Animal; // OK
new c2(); // ERROR: still an abstract-typed ctor

// Union of `typeof X` values: distributes through UnionT and rejects the
// abstract case (analogue of `union_constructor.js` with `typeof` instead of
// `Class<>`).
declare const u: typeof Animal | typeof Dog;
new u(); // ERROR: union may resolve to Animal at runtime
