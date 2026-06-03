// When `super` types to `any`, the inherited-name walk finds nothing
// and the not-inherited pass fires naturally.

declare const AnyBase: any;

class Sub extends AnyBase {
  override foo(): void {} // ERROR: `super` is `any`; no inherited name found
}
