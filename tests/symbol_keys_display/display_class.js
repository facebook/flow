// A within-file value-level `class` with two distinct `unique symbol` members
// keeps them as distinct named keys. `type-at-pos` over a structural view of the
// instance (an object spread, and a homomorphic mapped type) renders both keys
// as `[a]` / `[b]` rather than collapsing them into one `[symbol]` entry.

declare const Keys: {
  readonly a: unique symbol,
  readonly b: unique symbol,
};

class C {
  [Keys.a]: number = 1;
  [Keys.b]: string = 's';
  x: boolean = true;
}

declare const c: C;

const spread = {...c};
//    ^
// type-at-pos keeps `[a]` and `[b]` distinct.

declare const mapped: {[K in keyof C]: C[K]};
mapped;
//^
// type-at-pos keeps `[a]` and `[b]` distinct.
