// The inherited-name walk follows the full super chain. `C` may override
// a method declared on its grandparent `A`, even when intermediate `B`
// doesn't redeclare it.

class A {
  fromA(): number {
    return 1;
  }
}

class B extends A {}

class C extends B {
  override fromA(): number { // OK: inherited via B from A
    return 2;
  }
}

new C().fromA() as number; // OK
