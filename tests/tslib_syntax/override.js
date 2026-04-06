class Base {
  foo(): void {}
  bar: string = "";
}

class Derived extends Base {
  override foo(): void {} // ERROR - support to come later
  override bar: string = "x"; // ERROR - support to come later
}
