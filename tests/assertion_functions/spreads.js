declare function assertString(value: unknown): asserts value is string;
declare function assertSecondString(
  ignored: unknown,
  value: unknown,
): asserts value is string;
declare function assertSecondTruthy(ignored: unknown, value: unknown): asserts value;
declare function assertFirstWithRest(
  value: unknown,
  ...rest: Array<unknown>
): asserts value is string;
declare function assertTruthyValue<T>(value: ?T): asserts value;

function spreadBeforeAssertedIndexDoesNotRefine(s: Array<unknown>, value: unknown) {
  assertSecondString(...s, value);
  value as string; // error: spread shifts positional arguments
}

function spreadBeforeBareArgumentDoesNotRefine(s: Array<unknown>, value: ?string) {
  assertSecondTruthy(...s, value);
  value as string; // error: spread shifts positional arguments
}

function spreadBeforeMissingIndexDoesNotEndPath(s: Array<unknown>): number {
  assertSecondTruthy(...s);
  return 0;
}

function spreadBeforeFalseIndexDoesNotEndPath(s: Array<unknown>): number {
  assertSecondTruthy(...s, false);
  return 0;
}

function spreadAtAssertedIndexDoesNotEndPath(s: Array<?string>): Array<?string> {
  assertTruthyValue(...s);
  return s;
}

function trailingSpreadStillRefines(s: Array<unknown>, value: unknown) {
  assertFirstWithRest(value, ...s);
  value as string;
  value as number; // error: trailing spread does not shift the asserted index
}
