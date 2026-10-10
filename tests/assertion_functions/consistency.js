// The body of an assertion function must narrow the asserted parameter on
// every path that completes normally.

declare function assertString(value: unknown): asserts value is string;
declare function assertTruthy(value: unknown): asserts value;
declare function invariant(cond: unknown, message?: string): void;

function throwsWhenNotString(value: unknown): asserts value is string {
  if (typeof value !== 'string') {
    throw new Error();
  }
}

function callsOtherAssertion(value: unknown): asserts value is string {
  assertString(value);
}

function usesInvariant(value: unknown): asserts value is string {
  invariant(typeof value === 'string');
}

function bareThrows(value: unknown): asserts value {
  if (!value) {
    throw new Error();
  }
}

function bareCallsOtherAssertion(value: ?{...}): asserts value {
  assertTruthy(value);
}

function bareInvariant(value: string | 0 | null): asserts value {
  invariant(value);
}

function bareAny(value: any): asserts value {} // ok: `any` is not checked

function alwaysThrows(value: unknown): asserts value is string {
  throw new Error();
}

function returnsAfterNarrowing(value: unknown): asserts value is string {
  if (typeof value === 'string') {
    return;
  }
  throw new Error();
}

function returnsOtherAssertion(value: unknown): asserts value is string {
  return assertString(value);
}

const arrowCallsOtherAssertion = (value: unknown): asserts value is string =>
  assertString(value);

function switchWithThrowingDefault(value: 'a' | 'b' | 'c'): asserts value is 'a' | 'b' {
  switch (value) {
    case 'a':
    case 'b':
      return;
    default:
      throw new Error();
  }
}

function narrowedInTry(value: unknown): asserts value is string {
  try {
    assertString(value);
  } catch (e) {
    throw e;
  }
}

function nestedFunctionReturns(value: unknown): asserts value is string {
  const f = () => {
    return 1; // ok: belongs to the nested function
  };
  const g = () => 2; // ok: belongs to the nested function
  assertString(value);
}

class A {}
class B extends A {
  b: number = 0;
}

class C extends A {
  assertB(): asserts this is B {
    if (!(this instanceof B)) {
      throw new Error();
    }
  }
}

function emptyBody(value: unknown): asserts value is string {} // error: unknown ~> string

function bareEmptyBody(value: ?string): asserts value {} // error: may still be falsy

function wrongDirection(value: unknown): asserts value is string { // error: unknown ~> string
  if (typeof value === 'string') {
    throw new Error();
  }
}

function partialNarrowing(value: ?(string | number)): asserts value is string { // error: number ~> string
  if (value == null) {
    throw new Error();
  }
}

function earlyReturn(value: unknown, skip: boolean): asserts value is string {
  if (skip) {
    return; // error: unknown ~> string
  }
  assertString(value);
}

function reassigned(value: unknown): asserts value is string { // error: value is written to
  value = 'a';
}

function swallowed(value: unknown): asserts value is string { // error: catch path falls through unrefined
  try {
    assertString(value);
  } catch {}
}

function bareOnlyRemovesNull(value: ?boolean): asserts value { // error: may still be void or false
  if (value === null) {
    throw new Error();
  }
}

function bareEarlyReturn(value: ?string, skip: boolean): asserts value {
  if (skip) {
    return; // error: may still be falsy
  }
  assertTruthy(value);
}

const bareArrowDoesNotNarrow = (value: ?string): asserts value =>
  undefined; // error: may still be falsy

const arrowDoesNotNarrow = (value: unknown): asserts value is string =>
  undefined; // error: unknown ~> string

function havoced(value: unknown): asserts value is string { // error: value is havoced by `reset()`
  const reset = () => {
    value = 1;
  };
  assertString(value);
  reset();
  value;
}

function havocedWithoutLaterRead(value: unknown): asserts value is string { // TODO: should error like `havoced`
  const reset = () => {
    value = 1;
  };
  assertString(value);
  reset();
}

class D extends A {
  assertB(): asserts this is B {} // error: D ~> B
}
