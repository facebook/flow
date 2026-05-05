// Minimal reproduction of the enum union representative error dedup.

// Enum-like frozen object with number values
const Keys = Object.freeze({
  UP: 38,
  DOWN: 40,
  LEFT: 37,
  RIGHT: 39,
  HOME: 36,
  END: 35,
});

declare function createObjectFrom<TKey extends string>(
  keys: $ReadOnlyArray<TKey>,
): {[key: TKey]: boolean};

// Case 1: Array of 6 enum members
// Each array element has its own type, but TKey is inferred as the union.
// Q: Is TKey a single union type, or is each element checked separately?
const navigationKeys = createObjectFrom([
  Keys.UP,
  Keys.DOWN,
  Keys.LEFT,
  Keys.RIGHT,
  Keys.HOME,
  Keys.END,
]);

// Case 2: Smaller array - 2 enum members
const deleteKeys = createObjectFrom([Keys.HOME, Keys.END]);

// Case 3: Declared union type (large literals) -> representative
declare const k: 38 | 40 | 37;
declare function needsString(x: string): void;
needsString(k);

// Case 4: Declared union type (small literals) -> representative?
declare const nums: 1 | 2 | 3;
(nums as string);

// Case 5: Heterogeneous union - should NOT use representative
declare const mixed: 1 | "two" | true;
(mixed as null);

// Case 6: Homogeneous string literal union against number
declare const strs: "a" | "b" | "c";
(strs as number);

// Case 7: Union of two union types causes nested UnionRepresentative frames.
// When A | B has un-flattened UnionT members, the representative is itself
// a UnionT, which recursively triggers the optimization, producing doubled
// "at least one member of" messages.
type A = 'a1' | 'a2' | 'a3';
type B = 'b1' | 'b2' | 'b3';
declare function needsNumber(x: number): void;
declare var combined: A | B;
needsNumber(combined);

// Case 8: Enum union through the catch-all (UnionT, _) dispatch path.
// CallT is not specifically handled for unions, so it falls through
// to the catch-all which now applies the representative optimization.
declare const codes: 10 | 20 | 30;
codes(); // error: number is not callable, via catch-all
