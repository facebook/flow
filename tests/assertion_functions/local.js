declare function assertNumber(value: unknown): asserts value is number;
declare function assertTruthy<T>(value: ?T): asserts value;
declare function assertOptional(value?: unknown): asserts value;
declare function assertWithMessage(
  condition: boolean,
  message: string,
): asserts condition;

function assertWithDefault(value: unknown = true): asserts value {
  if (!value) {
    throw new Error('falsy');
  }
}
declare function assertSecond(
  ignored: unknown,
  value: unknown,
): asserts value is string;

const annotated: (value: unknown) => asserts value is number = assertNumber;
const inferred = assertNumber;
const inferredFromReturn = (value: unknown): asserts value is number => assertNumber(value);
const annotatedObject: {
  nested: {
    assertNumber(value: unknown): asserts value is number,
  },
} = {nested: {assertNumber}};
const inferredObject = {assertNumber};
const fromAlias: NumberAssertion = assertNumber;
const fromInterface: AssertionObject = {assertNumber};

type NumberAssertion = (value: unknown) => asserts value is number;
interface AssertionObject {
  assertNumber(value: unknown): asserts value is number;
}

function direct(value: unknown) {
  assertNumber(value);
  value as number;
  value as string; // error: assertion narrowed to number
}

function explicitVariable(value: unknown) {
  annotated(value);
  value as number;
  value as string; // error: assertion narrowed to number
}

function explicitObject(value: unknown) {
  annotatedObject.nested.assertNumber(value);
  value as number;
  value as string; // error: assertion narrowed to number
}

function namedAnnotations(value1: unknown, value2: unknown) {
  fromAlias(value1);
  value1 as number;
  value1 as string; // error: type-alias annotation is an assertion

  fromInterface.assertNumber(value2);
  value2 as number;
  value2 as string; // error: interface annotation is an assertion
}

function inferredAliasesAreAssertions(
  value1: unknown,
  value2: unknown,
  value3: unknown,
) {
  inferred(value1);
  value1 as number;
  value1 as string; // error: inferred alias narrows to number

  inferredObject.assertNumber(value2);
  value2 as number;
  value2 as string; // error: inferred object property narrows to number

  inferredFromReturn(value3);
  value3 as number;
  value3 as string; // error: return annotation propagates through the variable
}

function bare(value: ?string) {
  assertTruthy(value);
  value as string;
  value as null; // error: assertion removed null and void
}

function bareFalseIsAbrupt(): number {
  assertTruthy(false);
  return 0; // error: unreachable after a bare assertion of false
}

function bareFalseStillChecksArgumentTypes(): void {
  assertWithMessage(false, 42); // error: number is incompatible with string
}

function bareFalseStillChecksArity(): void {
  assertWithMessage(false); // error: missing required argument
}

function bareMissingArgumentIsReachable(): number {
  assertOptional();
  return 0; // no error: an omitted argument may hit a callee default, so the call can return
}

function bareDefaultedMissingArgumentIsReachable(): number {
  assertWithDefault();
  return 0; // no error: the default applies, so the call can return normally
}

function bareDefaultedMissingArgumentDoesNotEndSequence(): void {
  (assertWithDefault(), 'not a number' as number); // error: the assertion call may return
}

function bareDefaultedRefines(value: ?string) {
  assertWithDefault(value);
  value as string;
  value as null; // error: assertion removed null and void
}

function bareFalseClosesBranch(value: ?string): string {
  if (value == null) {
    assertTruthy(false);
  }
  return value;
}

function bareFalseClosesBranchInLoop(values: Array<?string>): void {
  for (const value of values) {
    if (value == null) {
      assertTruthy(false);
    }
    value as string;
  }
}

function parameterIndex(value: unknown) {
  assertSecond(null, value);
  value as string;
  value as number; // error: assertion narrowed to string
}

function expressionPositionDoesNotAssert(value: unknown) {
  const result = assertNumber(value);
  result as void;
  value as number; // error: only expression-statement calls assert
}

function assertionCallsDoNotHavoc(field: {name: ?string}, value: unknown) {
  if (field.name != null) {
    assertNumber(value);
    field.name as string;
    field.name as number; // error: refinement survives the assertion call
  }
}

function usedBeforeDeclaration(value: unknown) {
  later(value);
  value as number;
}

function later(value: unknown): asserts value is number {
  if (typeof value !== 'number') {
    throw new Error();
  }
}

class LocalAssertions {
  static assertNumber(value: unknown): asserts value is number {
    assertNumber(value);
  }

  assertString(value: unknown): asserts value is string {
    assertSecond(null, value);
  }
}

function staticMethod(value: unknown) {
  LocalAssertions.assertNumber(value);
  value as number;
  value as string; // error: annotated static method is an assertion
}

function instanceMethod(value: unknown) {
  const assertions: LocalAssertions = new LocalAssertions();
  assertions.assertString(value);
  value as string;
  value as number; // error: typed instance method is an assertion
}

function unannotatedBodyLocalSilent(value: unknown) {
  const localAlias = assertNumber;
  localAlias(value);
  value as number; // error: unannotated body-locals have no signature roots
  value as string; // error: unannotated body-locals have no signature roots
}

function inLoop(values: Array<unknown>) {
  for (const value of values) {
    assertNumber(value);
    value as number;
    value as string; // error: assertion applies inside loops
  }
}

function assertionInLoopPreservesPreLoopNarrowing(input: unknown, c: boolean) {
  let x = input;
  if (typeof x === 'number') {
    while (c) {
      assertNumber(x);
    }
    x as number;
    x as string; // error: the pre-loop narrowing is preserved after the loop
  }
  x = input; // Keep x mutable so loop scouting considers it for havoc.
}

type AssertionFor<T> = (value: unknown) => asserts value is T;

declare const genericAliasAssertion: AssertionFor<boolean>;

function genericAlias(value: unknown) {
  genericAliasAssertion(value);
  value as boolean;
  value as number; // error: instantiated alias is an assertion
}

type AssertionShape = {
  number: number,
  string: string,
};

declare const mappedAssertions: {
  [K in keyof AssertionShape]: (value: unknown) => asserts value is AssertionShape[K],
};

function mappedProperty(numberValue: unknown, stringValue: unknown) {
  mappedAssertions.number(numberValue);
  numberValue as number;
  numberValue as string; // error: mapped property narrows to its instantiated result

  mappedAssertions.string(stringValue);
  stringValue as string;
  stringValue as number; // error: each mapped property gets its own result type
}

type NestedAssertion<T> = {
  nested: {
    assert: (value: unknown) => asserts value is T,
  },
};

declare const nestedGenericAssertion: NestedAssertion<bigint>;

function nestedGenericProperty(value: unknown) {
  nestedGenericAssertion.nested.assert(value);
  value as bigint;
  value as number; // error: nested generic property is an assertion
}

declare function assertString(value: unknown): asserts value is string;

const methodObject = {
  assertNumber(value: unknown): asserts value is number {
    assertNumber(value);
  },
  nested: {
    assertString(value: unknown): asserts value is string {
      assertString(value);
    },
  },
};

function localObjectMethod(value1: unknown, value2: unknown) {
  methodObject.assertNumber(value1);
  value1 as number;
  value1 as string; // error: local object method is an assertion

  methodObject.nested.assertString(value2);
  value2 as string;
  value2 as number; // error: nested local object method is an assertion
}

function annotatedParameter(cb: NumberAssertion, value: unknown) {
  cb(value);
  value as number;
  value as string; // error: annotated parameter is an assertion
}

function unannotatedParameterSilent(cb, value: unknown) { // error: missing annotation on cb
  cb(value);
  value as number; // error: unannotated parameters have no signature roots
  value as string; // error: unannotated parameters have no signature roots
}

type RenamedNumberAssertion = (input: unknown) => asserts input is number;
type SecondParamAssertion = (first: unknown, second: unknown) => asserts second is number;

declare const agreedUnion: NumberAssertion | RenamedNumberAssertion;
declare const disagreedUnion: NumberAssertion | SecondParamAssertion;
declare const intersected: NumberAssertion & { extra: string };

function agreedUnionSilent(value: unknown) {
  agreedUnion(value);
  value as number; // error: union callees never classify, even when members agree
  value as string; // error: union callees never classify, even when members agree
}

function disagreedUnionSilent(value: unknown) {
  disagreedUnion(value);
  value as string; // error: disagreeing union members do not assert
}

function intersectionSilent(value: unknown) {
  intersected(value);
  value as string; // error: intersections never classify
}

function bareFalseWithTargsIsAbrupt(): number {
  assertTruthy<boolean>(false);
  return 0; // error: unreachable after a bare assertion of false
}

function localDeclareFunction(value: ?string) {
  declare function assertLocal(value: unknown): asserts value;
  assertLocal(value);
  value as string;
  value as null; // error: function-local declare function is an assertion
}

function localDeclareConst(value: unknown) {
  declare const assertLocal: (value: unknown) => asserts value is number;
  assertLocal(value);
  value as number;
  value as string; // error: function-local declare const is an assertion
}

function localFunction(value: unknown) {
  function assertLocal(value: unknown): asserts value is number {
    if (typeof value !== 'number') {
      throw new Error();
    }
  }
  assertLocal(value);
  value as number;
  value as string; // error: function-local function declaration is an assertion
}

function localShadowsModuleLevel(value: unknown) {
  declare function assertNumber(value: unknown): asserts value is string;
  assertNumber(value);
  value as string;
  value as number; // error: the local declaration shadows the module-level one
}

function localOverloadedSilent(value: unknown) {
  declare function assertLocal(value: unknown): asserts value is number;
  declare function assertLocal(value: unknown, message: string): void;
  assertLocal(value);
  value as number; // error: overloaded callees never classify
}
