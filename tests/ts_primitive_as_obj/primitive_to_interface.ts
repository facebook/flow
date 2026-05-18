// Primitive types (string, number, boolean) flowing into an interface.
// In .js files, Flow rejects this with EPrimitiveAsInterface ("a primitive,
// cannot be used as a subtype of interface type"). In .ts files we promote
// the primitive to its boxed-wrapper builtin (String, Number, Boolean) and
// fall through to structural subtyping, matching TS where primitives are
// assignable to any interface they structurally satisfy.

interface Empty {}
interface HasLength {
  length: number;
}
interface HasFoo {
  foo: number;
}

declare const s: string;
declare const n: number;
declare const b: boolean;
declare const bi: bigint;
declare const sym: symbol;

// Empty interface: accepted via wrapper promotion.
s as Empty; // OK
n as Empty; // OK
b as Empty; // OK
bi as Empty; // OK
sym as Empty; // OK

// Interface that the wrapper structurally satisfies.
s as HasLength; // OK -- String wrapper has `length`

// Interface the wrapper does not satisfy: structural subtyping still rejects,
// but with a missing-prop error rather than the blanket primitive-as-interface
// error.
s as HasFoo; // ERROR: `foo` missing on String wrapper
bi as HasFoo; // ERROR: `foo` missing on BigInt wrapper
sym as HasFoo; // ERROR: `foo` missing on Symbol wrapper
