// Primitive types (string, number, boolean) flowing into an inline object
// type. TypeScript treats interfaces and object types interchangeably, so the
// same wrapper-promotion that allows `string as Empty` (interface) should also
// allow `string as {}` (object type). In .js files Flow rejects this with a
// generic incompatible-type error; in .ts files we promote the primitive to
// its boxed-wrapper builtin (String, Number, Boolean) and fall through to
// structural subtyping.

declare const s: string;
declare const n: number;
declare const b: boolean;
declare const bi: bigint;
declare const sym: symbol;

// Empty object type: accepted via wrapper promotion.
s satisfies {}; // OK
n satisfies {}; // OK
b satisfies {}; // OK
bi satisfies {}; // OK
sym satisfies {}; // OK

// Object type that the wrapper structurally satisfies.
s satisfies {length: number}; // OK -- String wrapper has `length`

// Object type the wrapper does not satisfy: structural subtyping still
// rejects, but with a missing-prop error rather than a generic primitive-vs-
// object-type incompatibility.
s satisfies {foo: number}; // ERROR: `foo` missing on String wrapper
bi satisfies {foo: number}; // ERROR: `foo` missing on BigInt wrapper
sym satisfies {foo: number}; // ERROR: `foo` missing on Symbol wrapper
