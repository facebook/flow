// The concrete implementation of an *abstract* method does NOT require
// `override` (the abstract slot is the obligation, not an inherited
// concrete member to shadow). But the concrete shadow of a *concrete*
// method on the same abstract base DOES require `override`.

abstract class Base {
  abstract abs(): void;
  conc(): void {}
}

class Sub extends Base {
  abs(): void {} // OK: concrete impl of an abstract member is exempt
  conc(): void {} // ERROR: implicit override of `Base.conc` needs `override`
}

class Good extends Base {
  abs(): void {} // OK
  override conc(): void {} // OK
}
