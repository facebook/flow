class Base {
  show(): void {}
  hide(): void {}
}

class Sub extends Base {
  override show(): void {} // OK
  override hide(): void {} // OK
}

new Sub().show(); // OK
new Sub().hide(); // OK
