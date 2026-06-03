// Only members reachable through the `extends` chain count as override
// targets. Interface-only members do not.

interface I {
  fromIface(): void;
}

class Base {
  fromBase(): void {}
}

class Sub extends Base implements I {
  override fromBase(): void {} // OK: inherited from `Base`
  override fromIface(): void {} // ERROR: only on the `I` interface, not in `extends` chain
}
