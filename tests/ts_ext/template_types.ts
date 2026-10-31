// Non-ambient TS exports must preserve unbounded template patterns in signatures.
export type ParentSelector = `${string} &`;
export type Unbounded = `prefix-${string}`;

// Same-file checking must reject strings that do not match the template prefix.
const localGood: Unbounded = "prefix-value"; // OK
const localBad: Unbounded = "wrong-value"; // ERROR
