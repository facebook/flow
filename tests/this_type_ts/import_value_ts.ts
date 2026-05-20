// Bug 1: in a `.ts` file, value-import syntax (`import { I }`) of a
// type-only export must still preserve polymorphic `this` semantics for
// `implements`/`extends`. Without the `.ts`-fallback in
// `on_ModuleT_for_extends`, the cross-module raw-type recovery silently
// drops to `AnyT` here and the `this`-rebinding error never fires.
import { I } from './module';
import { IBox } from './module';

class GoodCM implements I {
  clone(): this { return this; }   // OK
}

class BadCM implements I {
  clone(): I { return this; }      // ERROR — must return `this`, not `I`
}

class GoodGen implements IBox<number> {
  value(): number { return 0; }
  clone(): this { return this; }   // OK
}

class BadGen implements IBox<number> {
  value(): number { return 0; }
  clone(): IBox<number> { return this; } // ERROR — must return `this`, not `IBox<number>`
}
