// `private abstract` is unimplementable: a subclass cannot access a
// `private` member, so the obligation can never be discharged. Reject the
// combination (matches TS1243). After the error, the obligation is dropped
// from the class signature so subclasses don't also get flagged for "missing
// implementation".

abstract class A {
  private abstract priv(): number; // ERROR: private+abstract method
  private abstract privField: number; // ERROR: private+abstract field
  abstract pub(): number;
}

class B extends A { // OK: `priv` / `privField` obligations were dropped, so B only owes `pub`
  pub(): number {
    return 0;
  }
}
new B().pub() as number; // OK

// Demonstrates the drop is selective: C still owes `pub` (which it doesn't
// implement) — if the drop had eaten public obligations too, this wouldn't
// flag.
class C extends A {} // ERROR: missing pub
