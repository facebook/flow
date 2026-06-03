// `override` neither suppresses nor introduces Flow's pre-existing shape
// and variance checks. The four subclasses below cross method/field/
// accessor shapes; each is annotated with the actual Flow outcome.

class Base {
  m(): number {
    return 0;
  }
  f: number = 0;
  get a(): number {
    return 0;
  }
  set b(v: number) {}
}

// method -> field-of-function-type. Flow accepts this (methods are
// structurally compatible with `() => T` fields).
class A extends Base {
  override m: () => number = () => 0; // OK
}

// field -> method. Fires variance (read-only narrowing) AND incompatible-type
// (function isn't assignable to the field's number type).
class B extends Base {
  override f(): number { // ERROR
    return 0;
  }
}

// getter -> field. Flow accepts this: reads still yield `number`, and the
// base has no setter to violate.
class C extends Base {
  override a: number = 0; // OK
}

// field -> getter. Fires variance: D's `f` is read-only but Base's is writable.
class D extends Base {
  override get f(): number { // ERROR
    return 0;
  }
}
