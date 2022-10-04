// @flow

const SynthesizableObject = {
  foo() { SynthesizableObject.foo(); this; }, // Illegal this. Methods don't bind this.
  arrow: () => { this; }, // Arrow functions don't bind this.
  // Function expression bind `this`, regardless of whether it is annotated.
  fun_expr: function (this: mixed) { this },
}
