// Optional calls narrow both branches when the declared types guarantee that
// the call runs. When an optional chain can skip the call, only the true branch
// narrows: a skipped call evaluates to `undefined`.

declare function isString(x: unknown): x is string;
declare function impliesString(x: unknown): implies x is string;
declare const maybeIsString: ?(x: unknown) => x is string;
declare const checks: {isString(x: unknown): x is string};
declare const maybeChecks: ?{isString(x: unknown): x is string};
declare const nested: {inner: ?{isString(x: unknown): x is string}};
declare const maybeNested: ?{inner: {isString(x: unknown): x is string}};
declare const maybeIsNumber: ?(x: unknown) => x is number;
declare const guardOrPlain: ?(((x: unknown) => x is string) | ((x: unknown) => boolean));

declare class Message {
  isText(): this is TextMessage;
}
declare class TextMessage extends Message {
  text: string;
}
declare class OtherMessage extends Message {
  other: number;
}

function nonNullableCallee(x: string | number) {
  if (isString?.(x)) {
    x as string; // OK
  } else {
    x as number; // OK
  }
}

function nullableCallee(x: string | number) {
  if (maybeIsString?.(x)) {
    x as string; // OK
  } else {
    x as number; // ERROR: the call might not run
  }
}

function nonNullableReceiver(x: string | number) {
  if (checks?.isString(x)) {
    x as string; // OK
  } else {
    x as number; // OK
  }
}

function nullableReceiver(x: string | number) {
  if (maybeChecks?.isString(x)) {
    x as string; // OK
  } else {
    x as number; // ERROR: the call might not run
  }
}

function nullableInChain(x: string | number) {
  if (nested.inner?.isString(x)) {
    x as string; // OK
  } else {
    x as number; // ERROR: the call might not run
  }
}

function nullableEarlierInChain(x: string | number) {
  if (maybeNested?.inner.isString(x)) {
    x as string; // OK
  } else {
    x as number; // ERROR: the call might not run
  }
}

function oneSided(x: string | number) {
  if (impliesString?.(x)) {
    x as string; // OK
  } else {
    x as number; // ERROR: one-sided guards do not refine the else branch
  }
}

function negated(x: string | number) {
  if (!isString?.(x)) {
    x as number; // OK
    return;
  }
  x as string; // OK
}

function conjunction(x: string | number, y: string | number) {
  if (isString?.(x) && checks?.isString(y)) {
    x as string; // OK
    y as string; // OK
  }
}

function receiverGuard(m: TextMessage | OtherMessage) {
  if (m?.isText()) {
    m.text as string; // OK
  } else {
    m as OtherMessage; // OK
  }
}

function nullableReceiverGuard(m: ?(TextMessage | OtherMessage)) {
  if (m?.isText()) {
    m.text as string; // OK
  } else if (m != null) {
    m as OtherMessage; // ERROR: the call might not run
  }
}

function nullableNegated(x: string | number) {
  if (!maybeIsString?.(x)) {
    x as number; // ERROR: the call might not run
    return;
  }
  x as string; // OK
}

function nullableConjunction(x: string | number, y: string | number) {
  if (maybeIsString?.(x) && maybeIsNumber?.(y)) {
    x as string; // OK
    y as number; // OK
  } else {
    x as number; // ERROR
    y as string; // ERROR
  }
}

function nullableDisjunction(x: string | number) {
  if (maybeIsString?.(x) || maybeIsNumber?.(x)) {
    x as string | number; // OK
    x as string; // ERROR
  } else {
    x as number; // ERROR: the calls might not run
  }
}

function nullableGuardOrPlain(x: string | number) {
  if (guardOrPlain?.(x)) {
    x as string; // ERROR: the plain alternative does not narrow
  }
}

function invalidatedByArgument(x: string | number) {
  if (isString?.(x)) {
    x = 1;
    x as string; // ERROR: reassigned
  }
}
