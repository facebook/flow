declare function assertFirst(value: unknown, ignored: unknown): asserts value is string;
declare function assertFirstBare(value: unknown, ignored: unknown): asserts value;
declare function assertSecond(
  ignored: unknown,
  value: unknown,
): asserts value is string;
declare function assertNamed(value: unknown, ignored: unknown): asserts value is {name: string};
declare function takesString(s: string): void;

function laterArgReassignsAssertedBinding(x: unknown, y: number) {
  assertFirst(x, (x = y));
  x as string; // error: x holds y (number); the assertion described the earlier value
  x as number;
}

function laterArgReassignsBare(x: ?string, y: null) {
  assertFirstBare(x, (x = y));
  x as string; // error: x is null; the truthiness assertion described the earlier value
  x as null;
}

let captured: unknown;
function reassignCaptured(): void {
  captured = 42;
}

function laterArgHavocsAssertedBinding(): void {
  captured = 'hello';
  assertFirst(captured, reassignCaptured());
  captured as string; // error: the later call reassigned the asserted binding
}

function pureLaterArgStillRefines(x: unknown, y: unknown) {
  assertFirst(x, y);
  x as string;
  x as number; // error: genuine narrowing is preserved
}

function earlierArgWriteStillRefines(x: unknown, w: unknown) {
  assertSecond((x = w), x);
  x as string;
  x as number; // error: earlier writes precede observation, narrowing is preserved
}

function laterArgReadUsesUnrefinedEnv(x: unknown) {
  // The inner `x` read resolves pre-refinement: replay applies
  // invalidation only. Re-recording it in the narrowed env used to
  // resolve the lazy assertion refinement re-entrantly and panic.
  assertFirst(x, takesString(x)); // error: unknown argument for string param
  x as string;
}

function laterArgMemberReadUsesUnrefinedEnv(x: {name: unknown}) {
  assertNamed(x, takesString(x.name)); // error: unknown argument for string param
  x.name as string;
}

function laterArgReadUsesUnrefinedEnvBare(x: string | null) {
  assertFirstBare(x, takesString(x)); // error: possibly-null argument for string param
  x as string;
}

function laterArgWritesOtherBindingFromAsserted(y: unknown, x: unknown) {
  assertFirst(y, (x = y));
  y as string;
  y as number; // error: assertion narrowing on y is preserved
  x as string; // error: x copies y's pre-refinement value; copies don't inherit narrowing
  x as number; // error: x holds y's earlier value, which is not a number
}

declare function assertCondition(condition: boolean, message: string): asserts condition;

function laterMessageReadsNullableValue(value: ?{name: string}): string {
  assertCondition(value != null, value.name); // ERROR
  return value.name;
}

type JSXProp = {
  value: null | {type: 'Literal', value: string} | {
    type: 'JSXExpressionContainer',
    expression: {type: 'Identifier', name: string} | {
      type: 'Literal', value: boolean | string,
    },
  },
};

function laterMessageReadsUnrefinedDiscriminatedUnion(prop: JSXProp): boolean {
  assertCondition(
    prop.value?.type === 'JSXExpressionContainer' &&
      prop.value.expression.type === 'Literal' &&
      typeof prop.value.expression.value === 'boolean',
    typeof prop.value.expression.value, // ERROR
  );
  return prop.value.expression.value;
}

function guardedMessageArgumentStillRefines(prop: JSXProp): boolean {
  const value =
    prop.value?.type === 'JSXExpressionContainer' &&
    prop.value.expression.type === 'Literal'
      ? prop.value.expression.value
      : undefined;
  assertCondition(typeof value === 'boolean', typeof value);
  return value;
}
