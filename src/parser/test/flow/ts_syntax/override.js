class Derived extends Base {
  override foo() {}
  override bar: string;
  override get baz() { return 1; }
  override set baz(v) {}
  static override qux() {}
}
