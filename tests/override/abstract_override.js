// `abstract override` — modifiers must appear in this order
// (`override abstract` is a parse error; see abstract_override_wrong_order.js).
//
// The override-check semantics treat `abstract override` identically to
// plain `override`: pass 1 (no-extends), pass 2 (not-inherited), and
// pass 3 (NIO) all apply the same way. Pass 3 skips abstract members
// (they're the abstract-obligation check's domain), so abstract members
// aren't required to carry `override` under NIO — but if they do, pass 2
// still requires the name to be inherited.

abstract class Base {
  inheritedMethod(): void {}
  inheritedField: string = "x";
}

abstract class Sub extends Base {
  abstract override inheritedMethod(): void; // OK
  abstract override inheritedField: string; // OK
  abstract override missingMethod(): void; // ERROR: not declared in `Base`
}

// `abstract override` on a class with no extends still fires no-extends.
abstract class NoExtends {
  abstract override foo(): void; // ERROR: class has no `extends` clause
}

// A concrete subclass can `override` either the originally-concrete member
// or the abstract-override on the intermediate parent.
class Concrete extends Sub {
  override inheritedMethod(): void {}
  override inheritedField: string = "y";
  override missingMethod(): void {} // OK: target is Sub's abstract member
}

new Concrete();
