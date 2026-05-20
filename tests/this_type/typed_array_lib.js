// `$TypedArrayInternal.copyWithin` returns `this`, so chaining preserves the
// concrete typed-array type instead of erasing to `void`.

declare const i32: Int32Array;
const chained: Int32Array = i32.copyWithin(0, 1).copyWithin(0, 2); // OK

// `fill`, `reverse`, and `sort` were already typed as `this`; verify
// chaining still works alongside `copyWithin`.
declare const u8: Uint8Array;
const u8b: Uint8Array = u8.fill(0).copyWithin(0, 1).reverse().sort(); // OK

// BigInt typed arrays use the same `$TypedArrayInternal` declaration
// parameterized by `bigint`, so chaining must work there too.
declare const bi64: BigInt64Array;
const bi64b: BigInt64Array = bi64.copyWithin(0, 1); // OK

// `set` correctly stays `: void` (per ECMA spec), so chaining off it errors.
declare const i16: Int16Array;
i16.set([1, 2, 3]).copyWithin(0, 1); // ERROR: void has no method copyWithin
