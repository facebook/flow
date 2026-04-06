declare class Derived extends Base {
  override foo(): void;
  override bar: string;
  override get baz(): number;
  override set baz(v: number): void;
  static override qux(): void;
}
