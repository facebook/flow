// `override` checks respect `unique symbol` member identity: overriding an
// inherited symbol member is matched to the base by the symbol's nominal
// identity, and `override` on a symbol the base doesn't declare is rejected.

declare const key1: unique symbol;
declare const key2: unique symbol;

class Base {
  [key1](): number {
    return 0;
  }
}

// Explicit `override` of an inherited symbol member -> OK (matched to `Base`'s
// `[key1]` by identity).
class Sub extends Base {
  override [key1](): number {
    return 1;
  }
}
declare const sub: Sub;
sub[key1]() as number; // OK

// `override` on a symbol member the base does not declare -> ERROR. `key2` is
// a distinct symbol from `key1`, so it is not found in `Base` (it would be
// wrongly accepted if distinct symbols collapsed to one key).
class SubBad extends Base {
  override [key2](): number { // ERROR: `[key2]` is not declared in the base class
    return 1;
  }
}

// `override` with no `extends` clause -> ERROR (no base to override from).
class NoExtends {
  override [key1](): number { // ERROR: class has no `extends` clause
    return 1;
  }
}
