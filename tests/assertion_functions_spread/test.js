declare function assertFirstBare(value: unknown): asserts value;
declare function assertSecondBare(ignored: unknown, value: unknown): asserts value;
declare function assertSecondGeneric<T>(ignored: T, value: unknown): asserts value;

function spreadBeforeAssertedIndexIsNotACondition(s: Array<unknown>, x: number | void) {
  assertSecondBare(...s, x); // no error: the spread shifts positions, so x is not the asserted argument
}

function directSecondArgIsACondition(x: number | void) {
  assertSecondBare(null, x); // error: sketchy null check on number
}

function spreadAtIndexZeroIsNotACondition(s: Array<unknown>, x: number | void) {
  assertFirstBare(...s, x); // no error: the asserted index holds a spread
}

function directSecondArgWithTargsIsACondition(x: number | void) {
  assertSecondGeneric<unknown>(null, x); // error: sketchy null check on number
}

function spreadWithTargsIsNotACondition(s: Array<unknown>, x: number | void) {
  assertSecondGeneric<unknown>(...s, x); // no error: spread shifts positions even with explicit type args
}
