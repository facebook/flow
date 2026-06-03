// `#foo` in a subclass is a separate identity from `#foo` in the base
// per ES6 private name semantics, so it does NOT count as an implicit
// override. NIO does not require the modifier on private shadows.

class Base {
  #foo(): void {}
  callFoo(): void {
    this.#foo();
  }
}

class Sub extends Base {
  #foo(): void {} // OK: separate private identity, not an override
  callMyFoo(): void {
    this.#foo();
  }
}
