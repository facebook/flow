class Base {
  setVisible(v: boolean): void {}
}

class Sub extends Base {
  override show(): void {} // ERROR: `show` is not declared in `Base`
}
