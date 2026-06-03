// `override` on static accessors. The per-side static override check
// reads the inherited static slot via the same walk as static methods/
// fields — getters and setters land in the static slot's prop map and
// should be reachable for the inherited-name check.

class Base {
  static get x(): number {
    return 1;
  }
  static set y(v: number) {}
}

class GoodGet extends Base {
  static override get x(): number { // OK
    return 2;
  }
}

class GoodSet extends Base {
  static override set y(v: number) {} // OK
}

class BadGet extends Base {
  static override get missing(): number { // ERROR: not declared in `Base`
    return 0;
  }
}

class BadSet extends Base {
  static override set missing(v: number) {} // ERROR: not declared in `Base`
}

Base.x as number;
GoodGet.x as number;
Base.y = 1;
GoodSet.y = 2;
