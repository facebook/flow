// Private methods and fields still get an unsound `this: any`
// (UnsoundnessKind::BoundFunctionThis in class_sig.rs), so extracting one is
// silent even under the new this typing. Negative case: this file documents the
// hole that is deliberately left open.

class C {
  #foo(): number {
    return 3;
  }
  static #bar(): string {
    return 'bar';
  }

  test(): void {
    this.#foo as () => number; // ok, `this` is `any`
    C.#bar as () => string; // ok, `this` is `any`
  }
}
