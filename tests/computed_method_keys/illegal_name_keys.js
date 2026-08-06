// @flow
// Literal computed method keys that carry no legal property name. Each one
// parses into a literal expression rather than a reference, so the checker
// rejects the name instead of chasing a value that is not there.
interface Nul {
  // `null` is the null literal, matching a `.d.ts` body and the expression
  // parser, not a reference to a name `null`, so this is an "Illegal name"
  // error rather than a spurious cannot-resolve-`null` error.
  [null](): number; // ERROR: a null literal names no property
}
interface Bool {
  [true](): number; // ERROR: a boolean literal names no property
}
interface Big {
  [1n](): number; // ERROR: a bigint literal names no property
}
declare const nul: Nul;
declare const bool: Bool;
declare const big: Big;
nul;
bool;
big;
