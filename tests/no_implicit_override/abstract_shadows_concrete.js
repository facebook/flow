// `abstract foo()` on a subclass shadowing a concrete inherited `foo()` is
// NOT an implicit override — abstract members are the abstract-obligation
// check's domain. The implicit-override check's concrete-name set must
// exclude abstract members (parallel to how check_abstract_obligations
// subtracts the abstract set).

class Base {
  foo(): void {}
}

abstract class C extends Base {
  abstract foo(): void; // OK — abstract members aren't implicit-override candidates
}

class D extends C {
  override foo(): void {} // OK — explicit override of the abstract
}

new D().foo();
