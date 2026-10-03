declare function unbound(x: unknown): asserts y is string; // error: name is not a parameter

declare function restParam(
  ...xs: Array<unknown>
): asserts xs is Array<string>; // error: rest parameters cannot be asserted

function destructured({x}: {x: unknown}): asserts x is string {} // error: destructured params have no root binding

declare function unboundBare(x: unknown): asserts y; // error: name is not a parameter

declare function widerThanParam(x: string): asserts x is number; // error: guard must be a subtype of parameter

