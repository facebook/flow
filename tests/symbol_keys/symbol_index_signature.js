// `keyof` over a `symbol` index signature is `symbol`, the same as TypeScript.
// Regression: the key was stringified on the way out, so `keyof` read as
// `string` while still only accepting a symbol on the way in.
declare const gen: symbol;
declare const s: unique symbol;

type Idx = {[k: symbol]: number};
declare const kg: keyof Idx;

const out1: symbol = kg; // OK: the key set is `symbol`
const out2: string = kg; // ERROR: it is `symbol`, not `string`

const in1: keyof Idx = gen; // OK: any symbol is a key of a symbol indexer
const in2: keyof Idx = s; // OK: a `unique symbol` is a symbol
const in3: keyof Idx = 'x'; // ERROR: a string is not

// A general `symbol` is only a key where a symbol indexer accepts it. It is not
// a key of an object with string keys, nor of one keyed by a specific symbol.
type StrOnly = {a: string};
const in4: keyof StrOnly = gen; // ERROR: no symbol indexer to accept it

type Named = {[s]: number};
const in5: keyof Named = gen; // ERROR: `[s]` is one specific key, not every symbol
const in6: keyof Named = s; // OK

// Where there is an indexer, its key type decides.
type StrIdx = {[k: string]: number};
const in7: keyof StrIdx = gen; // ERROR: that indexer takes strings

type NumIdx = {[k: number]: number};
const in8: keyof NumIdx = gen; // ERROR: that indexer takes numbers

type AnyKeyIdx = {[k: string | symbol]: number};
const in9: keyof AnyKeyIdx = gen; // OK
const in10: keyof AnyKeyIdx = 'x'; // OK

// Runtime enumeration still omits a symbol indexer, matching JavaScript.
declare const idx: Idx;
const ks: Array<empty> = Object.keys(idx); // OK: no runtime key

// A key written as a union is classified member by member, so a union of
// symbols is a symbol key set rather than a stringified one.
declare const t: unique symbol;
type UnionIdx = {[k: typeof s | typeof t]: number};
declare const ku: keyof UnionIdx;

const out3: typeof s | typeof t = ku; // OK: both symbols survive
const out4: typeof s = ku; // ERROR: the key set has both, not just `s`
const out5: string = ku; // ERROR: they are symbols, not strings

declare const uidx: UnionIdx;
const ks2: Array<empty> = Object.keys(uidx); // OK: no runtime key

// One union can hold both a symbol and a string, and then each half
// contributes on its own terms: the symbol as itself, the string stringified.
type StrOrSymIdx = {[k: string | typeof s]: number};
declare const km: keyof StrOrSymIdx;

const out6: string | typeof s = km; // OK
const out7: string = km; // ERROR: the symbol half is a key too

// Only the string half is enumerable at runtime.
declare const midx: StrOrSymIdx;
const ks3: Array<string> = Object.keys(midx); // OK

// A mapped type maps over the key set, so a symbol indexer stays a symbol
// indexer instead of turning into a string one.
type Mapped = {[K in keyof Idx]: boolean};
declare const mapped: Mapped;

const mv1: boolean = mapped[gen]; // OK: still keyed by symbol
const mv2: boolean = mapped['x']; // ERROR: a string is not a key
