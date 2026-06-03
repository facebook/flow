class Base {
  foo(): void {}
  bar: string = "";
}

class Derived extends Base {
  override foo(): void {} // OK
  override bar: string = "x"; // OK
}
