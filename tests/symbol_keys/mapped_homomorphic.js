// A homomorphic mapped type (no `as` remap clause) over a type with `unique
// symbol` members preserves the symbol keys, resolving each to its own member
// type rather than dropping them or collapsing them into a single indexer. This
// is the shape `Partial`, `$ReadOnly`, and `Pick` are built on.
import {Keys} from './keys';
import type {I} from './keys';

// An identity homomorphic mapped type keeps each symbol key distinct.
type M = {[K in keyof I]: I[K]};
declare const m: M;
m[Keys.a] as number; // OK
m[Keys.b] as string; // OK
m[Keys.a] as string; // ERROR: number is not string (key `a` kept distinct)

// `Partial` keeps the symbol keys and makes each optional.
declare const p: Partial<I>;
p[Keys.a] as (number | void); // OK: symbol key preserved, now optional
p[Keys.a] as number; // ERROR: the property is optional, so it may be void
