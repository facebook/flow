// A concrete member in a subclass must discharge an inherited abstract
// obligation of the same name. `check_abstract_obligations` consults
// `own_concrete_names`, so when the walker finds `x` in
// A.inst_abstract_props it is already satisfied.
//
// Note: `update_abstract_members` is add-only (an abstract decl in A
// never removes a name from anywhere). The in-class shadow case
// (`abstract x; x = 0;` inside one class) is parser-blocked as a
// duplicate-member error, so no remove path is needed.

abstract class A {
  abstract x: number;
}

class B extends A { // OK: B's concrete `x` discharges A's abstract `x`
  x: number = 0;
}

new B().x as number; // OK
