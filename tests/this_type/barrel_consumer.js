// Bug B regression: re-exported interface through a barrel module must
// preserve polymorphic `this` so `extends`/`implements` consumers can
// rebind across the re-export chain.

import type { IBarrel } from './barrel';

// extends-via-interface — sig-merge consumer reads the re-exported
// `IBarrel` from the barrel's type_exports_tmap. The
// `type_for_extends` slot carries the un-canonicalized form.
interface ChildOfBarrel extends IBarrel {
  extra: string;
}
declare const cb: ChildOfBarrel;
cb.clone() as ChildOfBarrel; // OK — `this` rebound across the barrel
cb.clone() as IBarrel; // OK — ChildOfBarrel is also an IBarrel

// implements — statement-typing consumer goes through
// `import_named_specifier_type_for_extends` against the barrel module,
// which routes through `on_ModuleT_for_extends` and reads the
// `type_for_extends` slot.
class GoodBarrel implements IBarrel {
  clone(): this { return this; }
}

class BadBarrel implements IBarrel {
  clone(): IBarrel { return this; } // ERROR — must return `this`, not `IBarrel`
}
