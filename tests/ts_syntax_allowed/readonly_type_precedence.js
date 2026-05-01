// `readonly` precedence — must bind tighter than `|`, `&`, and `extends`
// (conditional). Verify the parse succeeds AND the typing matches
// `ReadonlyArray<T>` per arm.

type F1 = readonly string[] | string;
type F2 = string | readonly string[] | undefined;
type F3 = string | readonly string[] | number;
type F4 = readonly string[] & {length: number};
type F5 = readonly [number, string] | null;

declare const f1: F1;
if (typeof f1 !== 'string') {
  f1[0] as string; // OK
  f1[0] = 'x'; // ERROR: not writable
}

declare const f5: F5;
if (f5 != null) {
  f5[0] as number; // OK
  f5[0] = 1; // ERROR: not writable
}

declare const f4: F4;
f4.length as number; // OK
f4[0] = 'x'; // ERROR: not writable

// Real react-hook-form shape: generic constrained by union containing `readonly T[]`.
declare function g<T extends string | readonly string[] | undefined>(x?: T): T;
g('hello'); // OK
g(['a', 'b']); // OK
g(undefined); // OK

// Conditional precedence: `readonly` must only consume the checkType, leaving
// `extends ... ? ... : ...` to associate at the conditional layer.
type IsArr<T> = T extends readonly any[] ? true : false;
const arr: IsArr<readonly string[]> = true; // OK: resolves to `true`
const notArr: IsArr<string> = false; // OK: resolves to `false`
