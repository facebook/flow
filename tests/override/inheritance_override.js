// Method parameter contravariance is a Flow-wide rule, not an
// override-specific check. Narrowing a parameter type via an
// `override` still trips Flow's pre-existing contravariance error,
// matching `tests/abstract_classes/inheritance_override.js`.

class A {
  m(x: string | number): void {}
}

class B extends A {
  override m(x: string): void {} // ERROR: narrowing parameter type
}

class C extends A {
  override m(x: string | number): void {} // OK
}
new C().m("hi"); // OK

class P {
  m(): this {
    return this;
  }
}

class Q extends P {
  override m(): this { // OK
    return this;
  }
}
new Q().m().m(); // OK
