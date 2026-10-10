// A statement-position call to an assertion function that the callee
// analysis cannot prove does not narrow, so it is reported (TS2775/TS2776).

declare function assertString(value: unknown): asserts value is string;
declare const utils: {assertString(value: unknown): asserts value is string};
declare function getAssert(): (value: unknown) => asserts value is string;

class C {
  assertString(value: unknown): asserts value is string {
    if (typeof value !== 'string') {
      throw new Error();
    }
  }

  viaThis(x: unknown) {
    this.assertString(x); // TODO: TS narrows `this` calls
  }
}

const topAlias = assertString;
const {assertString: topDestructured} = utils;

function recognized(
  x: unknown,
  f: (value: unknown) => asserts value is string,
  c: C,
) {
  assertString(x); // ok
  utils.assertString(x); // ok
  topAlias(x); // ok
  topDestructured(x); // ok
  f(x); // ok
  c.assertString(x); // ok
  const annotated: (value: unknown) => asserts value is string = assertString;
  annotated(x); // ok
  function local(value: unknown): asserts value is string {
    assertString(value);
  }
  local(x); // ok
}

function unannotatedLocal(x: unknown) {
  const g = assertString;
  g(x); // error: `g` needs an annotation
}

function unannotatedLet(x: unknown) {
  let g = assertString;
  g(x); // error: `g` needs an annotation
}

function unannotatedDestructuring(x: unknown) {
  const {assertString: g} = utils;
  g(x); // error: `g` needs an annotation
}

function unannotatedObject(x: unknown) {
  const o = {assertString};
  o.assertString(x); // error: `o` needs an annotation
}

function unannotatedGeneric(x: unknown) {
  const g = <T>(value: T): asserts value is T & string => {
    assertString(value);
  };
  g(x); // error: `g` needs an annotation
}

function inSequence(x: unknown) {
  const g = assertString;
  (g(x), g(x)); // error twice: `g` needs an annotation
}

function callResult(x: unknown) {
  getAssert()(x); // error: not a dotted name
}

function computedMember(x: unknown) {
  utils['assertString'](x); // error: not a dotted name
}

function newInstance(x: unknown) {
  new C().assertString(x); // error: not a dotted name
}

function notStatementPosition(x: unknown, b: boolean) {
  const g = assertString;
  b && g(x); // ok: would not narrow anyway
  const r = g(x); // ok
}

function optionalCalls(x: unknown, u: typeof utils | void) {
  assertString?.(x); // TODO: TS narrows optional calls
  u?.assertString(x); // ok: may not run
}

function notDefinitelyAssertions(
  x: unknown,
  union:
    | ((value: unknown) => asserts value is string)
    | ((value: unknown) => void),
  any: any,
) {
  union(x); // ok: unions never narrow
  any(x); // ok
  const typeGuard = (value: unknown): value is string =>
    typeof value === 'string';
  typeGuard(x); // ok: not an assertion
}

type StringAssertion = (value: unknown) => asserts value is string;
declare const intersected: StringAssertion & {extra: string};
declare function overloaded(value: unknown): asserts value is string;
declare function overloaded(value: unknown, message: string): void;

function unsupportedTypes<F extends StringAssertion>(x: unknown, g: F) {
  intersected(x); // error: intersection
  overloaded(x); // error: overloaded
  g(x); // error: type parameter (TODO: TS narrows through the bound)
}

function contextuallyTyped(x: unknown) {
  const h: (g: StringAssertion, x: unknown) => void = (g, x) => {
    g(x); // error: `g` needs an annotation
  };
}

declare function assertionFirst(value: number): asserts value is number;
declare function assertionFirst(value: unknown): void;
declare function assertionLast(value: number): void;
declare function assertionLast(value: unknown): asserts value is string;

function overloadWinners(x: unknown) {
  assertionFirst(x); // ok: the assertion overload loses
  assertionLast(x); // error: the assertion overload wins
  utils['assertString'](x); // error: previous speculation does not suppress this call
}

function assertionUnions(
  x: unknown,
  allAssertions:
    | ((value: unknown) => asserts value is string)
    | ((value: unknown) => asserts value is number),
) {
  allAssertions(x); // ok: union callees do not narrow
}
