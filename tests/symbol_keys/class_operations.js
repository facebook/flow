// Object-kit and `keyof` operations over a within-file value-level `class` with
// `unique symbol` members. The class instance type carries each symbol member
// with its own nominal identity, so `Omit`, `Pick`, a homomorphic mapped type,
// and `keyof` treat them as distinct named keys rather than collapsing them into
// a single indexer.

declare const a: unique symbol;
declare const b: unique symbol;

class C {
  [a]: number = 1;
  [b]: string = 's';
  x: boolean = true;
}

// `Omit` drops only the symbol key `[a]`, keeping `[b]` and `x`.
type O = Omit<C, typeof a>;
declare const o: O;
o.x as boolean; // OK
o[b] as string; // OK
o[a]; // ERROR: [a] removed

// `Pick` keeps only the symbol key `[a]`.
type P = Pick<C, typeof a>;
declare const p: P;
p[a] as number; // OK
p[b]; // ERROR: [b] not picked

// A homomorphic mapped type keeps each symbol key distinct, resolving each to
// its own member type.
type M = {[K in keyof C]: C[K]};
declare const m: M;
m[a] as number; // OK
m[b] as string; // OK
m[a] as string; // ERROR: number is not string (a did not merge with b)

// `keyof C` includes each distinct symbol key: a matching symbol satisfies it,
// a distinct one is rejected.
a as keyof C; // OK
b as keyof C; // OK
declare const other: unique symbol;
other as keyof C; // ERROR: `other` is not a key of `C`
