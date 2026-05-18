// Repro of the `T | (string & {})` widening idiom commonly used in TS
// libraries to widen a literal union to `string` while preserving editor
// autocomplete on the literal members.
//
// Flow used to read the `{}` operand as an "object type" upper bound, so any
// string literal flowing into `string & {}` failed with "string literal X, a
// primitive, cannot be used as a subtype of object type". The wrapper-
// promotion in this directory's other tests makes the `string -> {}` half of
// the intersection succeed via the String builtin, so the intersection as a
// whole accepts the literal.

// Direct widening idiom.
type StringAutocomplete<T extends string> = T | (string & {});

declare function pickColor(c: StringAutocomplete<'red' | 'green' | 'blue'>): void;

pickColor('red'); // OK -- in the literal union
pickColor('blue'); // OK -- in the literal union
pickColor('papayawhip'); // OK -- accepted by `string & {}`

// Mapped-type-over-generic-keys variant: a recipe-style shape with conditions
// drawn from a literal union widened with `string & {}`.
type Conditions = 'sm' | 'md' | 'lg' | (string & {});
type Recipe<C extends string> = {[K in C]?: number};

declare const r: Recipe<Conditions>;

const a: number | void = r.sm; // OK -- declared key
const b: number | void = r['custom']; // OK -- arbitrary string via `string & {}`
