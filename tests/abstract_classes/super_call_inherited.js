// check_super_abstract must walk the full super chain: an abstract
// intermediate that doesn't redeclare `m` must not mask an
// `AbstractSuperCall` against a grandparent's abstract `m`.

abstract class A {
  abstract m(): number;
  concrete(): number {
    return 0;
  }
}

abstract class B extends A {
  // B does NOT redeclare m, so B.inst_abstract_props = {}.
  // Without the walker fix, super.m() from C wouldn't be flagged.
}

class C extends B {
  m(): number {
    return 1;
  }
  bad(): number {
    return super.m(); // ERROR: super.m() dispatches to A's abstract m
  }
  good(): number {
    return super.concrete(); // OK
  }
}

// super write must also be checked.
abstract class P {
  abstract x: number;
}
class Q extends P {
  x: number = 0;
  set_x(): void {
    super.x = 5; // ERROR: super.x = e on abstract prop
  }
}
