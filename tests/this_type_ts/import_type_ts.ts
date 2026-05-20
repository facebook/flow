// Control: explicit `import type` should still work for `implements` —
// regression check that the regular `_for_extends` path is intact.
import type { I } from './module';

class GoodCMT implements I {
  clone(): this { return this; }   // OK
}

class BadCMT implements I {
  clone(): I { return this; }      // ERROR — must return `this`, not `I`
}
