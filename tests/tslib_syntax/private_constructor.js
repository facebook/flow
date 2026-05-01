// `private constructor` must not be silently dropped from the type signature:
// if it were, the merge layer would synthesize a default zero-arg constructor
// and `new Singleton()` would spuriously type-check.
class Singleton {
  static instance: ?Singleton;
  private constructor(x: number) {}
  static get(): Singleton {
    if (Singleton.instance == null) Singleton.instance = new Singleton(0);
    return Singleton.instance;
  }
}

new Singleton(); // ERROR: requires `x` arg
new Singleton(1); // OK (private leaks externally — matches existing .d.ts behavior)
Singleton.get(); // OK
