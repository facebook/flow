// check_abstract_obligations must credit concrete implementations
// on INTERMEDIATE ancestors — `class C extends B {}` should not be
// flagged as missing `m` since B already implements it.

abstract class A {
  abstract m(): number;
  abstract n(): string;
}

class B extends A {
  m(): number {
    return 1;
  }
  n(): string {
    return "b";
  }
}

class C extends B {} // OK: C inherits B's concrete m and n

class D extends B { // OK: overrides m, inherits n from B
  m(): number {
    return 2;
  }
}

// ERROR: introduces a new abstract obligation but doesn't implement it.
abstract class E extends A {
  abstract extra(): boolean;
}
class F extends E { // ERROR: missing `extra`
  m(): number {
    return 1;
  }
  n(): string {
    return "f";
  }
}
