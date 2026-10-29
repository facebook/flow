declare function assertString(x: unknown): asserts x is string;
declare function assertTruthy(x: unknown): asserts x;
import {assertNumber, importedMappedAssertions} from './exports';
import * as all from './exports';

type Checks = {
  assertString: (x: unknown) => asserts x is string,
  assertTruthy: (x: unknown) => asserts x,
};
declare const checks: Checks;
declare const nested: {checks: Checks};
declare const nullableChecks: Checks | void;
declare const nullableNested: {checks: Checks | void};
declare const nullableAssert: ((x: unknown) => asserts x is string) | void;

function optionalFunction(x: unknown) {
  assertString?.(x);
  x as string;
  x as number; // ERROR
}

function optionalMethod(x: unknown) {
  checks.assertString?.(x);
  x as string;
  x as number; // ERROR
}

function optionalReceiver(x: unknown) {
  checks?.assertString(x);
  x as string;
  x as number; // ERROR
}

function optionalChain(x: unknown) {
  nested?.checks?.assertString?.(x);
  x as string;
  x as number; // ERROR
}

function skippedCalls(x: unknown, y: unknown, z: unknown) {
  nullableAssert?.(x);
  x as string; // ERROR
  nullableChecks?.assertString(y);
  y as string; // ERROR
  nullableNested.checks?.assertString(z);
  z as string; // ERROR
}

function optionalBare(x: string | null) {
  assertTruthy?.(x);
  x as string;
  x as null; // ERROR
}

function optionalCondition(x: unknown) {
  checks?.assertTruthy(typeof x === 'string');
  x as string;
  x as number; // ERROR
}

function optionalFalse(): empty {
  assertTruthy?.(false);
}

function skippedFalse(): number { // ERROR
  nullableChecks?.assertTruthy(false);
}

function optionalExpression(x: unknown) {
  const result = assertString?.(x);
  x as string; // ERROR
}

function optionalSequence(x: unknown, y: unknown) {
  assertString?.(x), checks?.assertString(y);
  x as string;
  y as string;
}

function optionalImports(x: unknown, y: unknown, z: unknown) {
  assertNumber?.(x);
  x as number;
  x as string; // ERROR
  all?.assertNumber?.(y);
  y as number;
  y as string; // ERROR
  // Mapped properties may be optional, so optional chains through them do not narrow.
  importedMappedAssertions?.number?.(z);
  z as number; // ERROR
}

declare const assertFirst: (x: unknown, ignored: unknown) => asserts x is string;
function laterArgument(x: unknown) {
  assertFirst?.(x, (x = 42));
  x as string; // ERROR
}

declare class OptionalBase {
  assertString(x: unknown): asserts x is string;
}

class OptionalDerived extends OptionalBase {
  viaOptional(x: unknown) {
    this?.assertString?.(x);
    x as string;
    x as number; // ERROR
  }
}

declare class Message {
  assertText(): asserts this is TextMessage;
}

declare class TextMessage extends Message {
  text: string;
}

function optionalAssertionReceiver(message: Message) {
  message?.assertText?.();
  message.text as string;
  message.text as number; // ERROR
}

function nullableAssertionReceiver(message: Message | void) {
  message?.assertText();
  message as TextMessage; // ERROR: assertion can be skipped
}

declare const intersectedChecks: Checks & {extra: number};
declare const overloadedAssert: {
  (x: unknown): asserts x is string,
  (x: unknown, y: unknown): asserts x is string,
};

// Optional calls that cannot narrow are not reported.
function unrecognizedOptional(x: unknown, y: unknown) {
  intersectedChecks?.assertString(x);
  x as string; // ERROR
  overloadedAssert?.(y);
  y as string; // ERROR
}

type MappedShape = {str: string};
declare const optionalMapped: {[K in keyof MappedShape]?: (v: unknown) => asserts v is MappedShape[K]};
declare const requiredMapped: {[K in keyof MappedShape]: (v: unknown) => asserts v is MappedShape[K]};
declare const optionalMappedReceiver: {[K in 'inner']?: Checks};

function optionalMappedProperties(x: unknown, y: unknown, z: unknown) {
  optionalMapped.str?.(x);
  x as string; // ERROR: optionalMapped.str can be missing
  requiredMapped.str?.(y);
  y as string; // ERROR
  optionalMappedReceiver.inner?.assertString(z);
  z as string; // ERROR: optionalMappedReceiver.inner can be missing
}
