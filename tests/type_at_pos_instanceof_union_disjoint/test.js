// @flow

// Regression test: refining a union by a class guard (`instanceof` or a `match`
// class pattern) must not leak `any` from the type arguments of provably-disjoint
// union members.

class Foo<F> {
  f: F;
  constructor(f: F) {
    this.f = f;
  }
}
class Bar<B> {
  b: B;
  constructor(b: B) {
    this.b = b;
  }
}

// `Bar<number>` is disjoint from `Foo`, so it is pruned; the refinement is
// `Foo<string>`, not `Foo<string> | Foo<any>`.
function test_instanceof(fob: Foo<string> | Bar<number>) {
  if (fob instanceof Foo) {
    fob;
//  ^
  }
}

// Same behavior through the `match` class-pattern path.
function test_match(fob: Foo<string> | Bar<number>) {
  match (fob) {
    Foo {...} as v => {
      v;
//    ^
    }
    Bar {...} as w => {
      w;
//    ^
    }
  };
}

// Downcast: `Base` is a superclass of `Derived`; a `Base`-typed union member
// must still refine to `Derived` (it is NOT pruned).
class Base {}
class Derived extends Base {}
function test_downcast(x: Base | string) {
  if (x instanceof Derived) {
    x;
//  ^
  }
}

// Single (non-union) scrutinee: keep the conservative guard type rather than
// `empty`, to avoid surfacing `empty` to users.
function test_single(b: Bar<number>) {
  if (b instanceof Foo) {
    b;
//  ^
  }
}

// Union where EVERY member is disjoint from the guard: pruning them all would
// yield a bare `empty`, so we fall back to the conservative guard type (as for a
// lone scrutinee) rather than pruning.
function test_all_disjoint(x: Bar<number> | string) {
  if (x instanceof Foo) {
    x;
//  ^
  }
}

// Same all-disjoint fall-back through the `match` class-pattern path.
function test_all_disjoint_match(x: Bar<number> | string) {
  match (x) {
    Foo {...} as v => {
      v;
//    ^
    }
    _ => {}
  };
}
