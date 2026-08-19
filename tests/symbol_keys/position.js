// A `unique symbol` annotation *is* the identity of one symbol, so it only
// means something where it stands for exactly one value. Anywhere else a single
// annotation is shared by every value written against it, which makes distinct
// runtime symbols compare equal and lets a lookup resolve through the wrong key.

// Allowed: a `const` binds one value, once.
declare const ok1: unique symbol;

// A rebindable variable could be pointed at another symbol, so it must be
// `const`, as TypeScript requires (TS1332).
declare let bad1: unique symbol; // ERROR: not `const`
declare var bad2: unique symbol; // ERROR: not `const`
let bad3: unique symbol; // ERROR: not `const`

// Allowed: a read-only object type or interface property declares one member.
// A writable one could be handed a different symbol, so it must be read-only,
// as TypeScript requires (TS1330).
declare const ok2: {readonly a: unique symbol};
declare const bad4: {c: unique symbol}; // ERROR: not read-only
interface OkI {
  readonly d: unique symbol;
  e: unique symbol; // ERROR: not read-only
}

// A computed key names one property just as a written one does, so the same
// rule applies to it, in every body that can carry one.
declare const ok3: {readonly [ok1]: unique symbol};
declare const bad5: {[ok1]: unique symbol}; // ERROR: not read-only
declare const ok4: {readonly ['lit']: unique symbol};
interface OkCI {
  readonly [ok1]: unique symbol;
  [ok1]: unique symbol; // ERROR: not read-only
}

// An optional property still declares the one member it declares.
declare const ok5: {readonly o?: unique symbol};

// A getter runs on every read and a set of members share one indexer, so
// neither stands for a single value.
declare const bad6: {get g(): unique symbol}; // ERROR
declare const bad7: {readonly [key: string]: unique symbol}; // ERROR

// A class is the exception to "any property will do". A static field is one
// value on the one class object, but an instance field is one per instance and
// a single annotation would be shared by all of them. TypeScript draws the same
// line, requiring a class property that spells one out to be both `static` and
// `readonly` (TS1331). Both class kinds are treated alike.
declare class OkC {
  static readonly f: unique symbol;
  static g: unique symbol; // ERROR: not `static readonly`
  readonly h: unique symbol; // ERROR: not `static readonly`
  static readonly [ok1]: unique symbol;
  readonly [ok2.a]: unique symbol; // ERROR: not `static readonly`
}
class OkRC {
  static readonly i: unique symbol;
  static j: unique symbol; // ERROR: not `static readonly`
  readonly k: unique symbol; // ERROR: not `static readonly`
  static readonly [ok1]: unique symbol;
}

// A return type is the unsound case: every call would yield "the same" symbol
// while returning a fresh one at runtime.
declare function mkRet(): unique symbol; // ERROR

// A type alias is the same hazard spelled differently, since every use of the
// alias refers to the one annotation.
type Alias = unique symbol; // ERROR

// Nested positions are rejected for the same reason: every element, member, or
// branch would be the one symbol.
declare const arr: Array<unique symbol>; // ERROR
declare const un: unique symbol | string; // ERROR
declare const nested: {a: Array<unique symbol>}; // ERROR
class NestedRC {
  static readonly l: Array<unique symbol>; // ERROR: nested even where a bare one is allowed
}

// A parameter is not inhabitable by anything, since no value can be declared
// with that annotation.
declare function takes(p: unique symbol): void; // ERROR

// A destructuring pattern's annotation is shared by every name it binds, so it
// is not a position that stands for one value however many names it declares.
const {m}: unique symbol = null as any; // ERROR
const [n]: unique symbol = null as any; // ERROR

// A loop binding takes a different value on each iteration however it is
// declared, as TypeScript requires (TS1334). Nothing can inhabit the
// annotation either, so each head also reports its element type.
declare const syms: Array<symbol>;
for (const p: unique symbol of syms) { // ERROR: loop binding, and `symbol` is not it
  p;
}
for (const q: unique symbol in {}) { // ERROR: loop binding, and a key is a string
  q;
}

// `typeof` a binding that was declared properly refers to that one symbol, so
// it stays usable in every position.
type ViaTypeof = typeof ok1;
declare function mkOk(): typeof ok1;
declare const arrOk: Array<typeof ok1>;
declare const o: {[ok1]: number};
const read: number = o[mkOk()]; // OK: still the symbol `ok1` names

// Every allowed position yields a symbol that works as a key, and each one is
// its own symbol: `ok2.a` cannot be read through `OkC.f`.
declare const byProp: {[ok2.a]: number};
const readProp: number = byProp[ok2.a];
declare const byStatic: {[OkC.f]: number};
const readStatic: number = byStatic[OkC.f];
const crossed: number = byProp[OkC.f]; // ERROR: a different symbol
