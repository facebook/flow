type Asserts = (x: unknown) => asserts x is string;
type Guard = (x: unknown) => x is string;
type Implies = (x: unknown) => implies x is string;
type Plain = (x: unknown) => boolean;

declare var assertion: Asserts;
declare var guard: Guard;
declare var implies: Implies;
declare var plain: Plain;

assertion as Asserts;
assertion as Guard; // error: assertion and conditional guards are distinct
assertion as Implies; // error: assertion and one-sided guards are distinct
assertion as Plain; // error: assertions return void
guard as Asserts; // error: conditional guard is not an assertion
implies as Asserts; // error: one-sided guard is not an assertion
plain as Asserts; // error: plain function is not an assertion

type BareAsserts = (x: unknown) => asserts x;
declare var bare: BareAsserts;

bare as BareAsserts;
bare as Asserts; // error: truthiness assertion and typed assertion differ
assertion as BareAsserts; // error: typed assertion and truthiness assertion differ

type AssertsSecond = (x: unknown, y: unknown) => asserts y is string;
type AssertsFirst = (x: unknown, y: unknown) => asserts x is string;
declare var second: AssertsSecond;

second as AssertsFirst; // error: asserted parameter positions differ

// Only the second member matches; the first member's mismatch is speculative.
assertion as ((x: string) => x is string) | ((x: string) => asserts x is string);
bare as ((x: string) => asserts x is string) | ((x: string) => asserts x);

declare class AssertedValue {
  readonly value: string | number;
}

declare class AssertedString extends AssertedValue {
  readonly value: string;
}

declare class TypedReceiverAssertion {
  check(x: unknown): asserts this is AssertedValue;
}

declare class StrongerReceiverAssertion extends TypedReceiverAssertion {
  check(x: unknown): asserts this is AssertedString;
}

declare class SameReceiverAssertion extends StrongerReceiverAssertion {
  check(renamed: unknown): asserts this is AssertedString;
}

declare class WeakerReceiverAssertion extends StrongerReceiverAssertion {
  check(x: unknown): asserts this is AssertedValue; // ERROR
}

declare class BareReceiverAssertion {
  check(x: unknown): asserts this;
}

declare class BareParameterAssertion {
  check(x: unknown): asserts x;
}

declare class TypedParameterAssertion {
  check(x: unknown): asserts x is AssertedString;
}

declare class ParameterInsteadOfReceiver extends BareReceiverAssertion {
  check(x: unknown): asserts x; // ERROR
}

declare class ReceiverInsteadOfParameter extends BareParameterAssertion {
  check(x: unknown): asserts this; // ERROR
}

declare class TypedParameterInsteadOfReceiver extends StrongerReceiverAssertion {
  check(x: unknown): asserts x is AssertedString; // ERROR
}

declare class TypedReceiverInsteadOfParameter extends TypedParameterAssertion {
  check(x: unknown): asserts this is AssertedString; // ERROR
}

declare class TypedInsteadOfBareReceiver extends BareReceiverAssertion {
  check(x: unknown): asserts this is AssertedString; // ERROR
}

declare class BareInsteadOfTypedReceiver extends StrongerReceiverAssertion {
  check(x: unknown): asserts this; // ERROR
}

declare class TypedReceiverInsteadOfBareParameter extends BareParameterAssertion {
  check(x: unknown): asserts this is AssertedString; // ERROR
}

declare class BareParameterInsteadOfTypedReceiver extends StrongerReceiverAssertion {
  check(x: unknown): asserts x; // ERROR
}
