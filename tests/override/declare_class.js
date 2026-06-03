// `declare class` participates in the override checks identically.
// The implicit-override pass (NIO) is suppressed on declare-class bodies,
// but the explicit-`override` checks still fire.

declare class Base {
  meth(): void;
}

declare class GoodDecl extends Base {
  override meth(): void; // OK: inherited from `Base`
}

declare class BadDecl extends Base {
  override missing(): void; // ERROR: not declared in `Base`
}

declare class NoExtendsDecl {
  override foo(): void; // ERROR: class has no `extends` clause
}
