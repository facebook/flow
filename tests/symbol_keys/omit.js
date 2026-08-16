// `Omit<O, typeof s>` must remove only the symbol key `s`, keeping the other
// keys. Regression: a `unique symbol` mapped-type key was classified as an
// indexer, so `{[K in typeof a]: mixed}` became a symbol dictionary matching
// every key, and `Omit` dropped every property.
import {Keys} from './keys';
type T = {[Keys.a]: number, [Keys.b]: string, x: boolean};

type O = Omit<T, typeof Keys.a>;
declare const o: O;
o.x as boolean; // OK: x kept
o[Keys.b] as string; // OK: [b] kept
o[Keys.a]; // ERROR: [a] removed

// A non-homomorphic mapped type over a single symbol key is a NAMED key, not a
// symbol indexer: a distinct symbol must not resolve through it.
type P = {[K in typeof Keys.a]: number};
declare const p: P;
p[Keys.a] as number; // OK
p[Keys.b]; // ERROR: only `[a]` is a key of P (would pass if it were an indexer)
