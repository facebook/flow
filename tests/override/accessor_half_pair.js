// Override semantics: the inherited-name check is name-only — if the
// base declares any accessor half (get or set), an `override` of either
// half on the subclass passes the override check (no `[invalid-override]`
// fires below).
//
// Note: Flow's pre-existing get/set variance check rejects half-pair
// inheritance because the subclass narrows access (read-only vs
// write-only). The two `[incompatible-variance]` errors below come
// from that check, NOT from the override modifier — they would fire
// the same way without `override`.

class Base {
  set x(v: number) {}
  get y(): number {
    return 1;
  }
}

class Sub extends Base {
  override get x(): number { // ERROR (Flow variance, not override-related)
    return 1;
  }
  override set y(v: number) {} // ERROR (Flow variance, not override-related)
}
