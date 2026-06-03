// `#foo` in a subclass is a separate identity from `#foo` in the base
// (per ES6 private name semantics), so `override #foo` can never refer
// to a base member.

class Base {
  #foo(): void {}
  callFoo(): void {
    this.#foo();
  }
}

class Sub extends Base {
  override #foo(): void {} // ERROR: `#foo` is a separate identity from Base's `#foo`
}
