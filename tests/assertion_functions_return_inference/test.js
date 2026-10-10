declare function assert(condition?: unknown, message?: string): asserts condition;
declare function assertSecond(message: unknown, condition: unknown): asserts condition;
declare function assertGeneric<T>(condition: T, ...rest: Array<unknown>): asserts condition;
declare function assertType(condition: unknown): asserts condition is number;
declare const optionalAssert: ?((condition: unknown) => asserts condition);

function catchRepro(promise: Promise<number>) {
  const result = promise.catch(() => {
    assert(false, 'Could not get query data');
  });
  result as Promise<number>;
  result as Promise<void>; // ERROR
}

function throwingFunctions() {
  function declaration() { assert(false); }
  declaration as () => empty;

  const arrow = () => { assert(false); };
  arrow as () => empty;

  const expression = function() { assert(false); };
  expression as () => empty;

  const methods = {
    fail() { assert(false); },
    arrow: () => { assert(false); },
    nested: { fail() { assert(false); } },
  };
  methods.fail as () => empty;
  methods.arrow as () => empty;
  methods.nested.fail as () => empty;

  const second = () => { assertSecond('failed', false); };
  second as () => empty;

  const generic = () => { assertGeneric<boolean>(false, ...[]); };
  generic as () => empty;

  const asyncArrow = async () => { assert(false); };
  asyncArrow as () => Promise<empty>;
}

function returningFunctions(condition: boolean, spread: Array<unknown>) {
  const sometimes = () => { if (condition) { assert(false); } };
  sometimes as () => void;
  sometimes as () => empty; // ERROR

  const trueArgument = () => { assert(true); };
  trueArgument as () => void;
  trueArgument as () => empty; // ERROR

  const unknownArgument = () => { assert(condition); };
  unknownArgument as () => void;
  unknownArgument as () => empty; // ERROR

  const missingArgument = () => { assert(); };
  missingArgument as () => void;
  missingArgument as () => empty; // ERROR

  const otherParameter = () => { assertSecond(false, condition); };
  otherParameter as () => void;
  otherParameter as () => empty; // ERROR

  const precedingSpread = () => { assertSecond(...spread, false); };
  precedingSpread as () => void;
  precedingSpread as () => empty; // ERROR

  const typeGuard = () => { assertType(false); };
  typeGuard as () => void;
  typeGuard as () => empty; // ERROR

  const optional = () => { optionalAssert?.(false); };
  optional as () => void;
  optional as () => empty; // ERROR

  const nested = () => {
    const fail = () => { assert(false); };
  };
  nested as () => void;
  nested as () => empty; // ERROR
}
