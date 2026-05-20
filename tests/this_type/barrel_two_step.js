// Bug 2: two-step barrel re-export (`import type { X } from './base';
// export type { X };`) must preserve the `t_for_extends` slot through
// `Pack.ExportTypeRef -> Pack.RemoteRef`. Without that thread, the
// downstream `implements`/`extends` consumer falls back to the
// canonicalized `type_` (which has been run through `fix_this_instance`),
// destroying polymorphic `this`.
//
// `barrel_base.js` already exercises the *one-step* form
// (`export type { Foo } from './source'`) via `Pack.ExportTypeFrom`. This
// file exercises the *two-step* form via `Pack.ExportTypeRef RemoteRef`.

import type { IBarrel } from './barrel_two_step_relay';

class GoodTwoStep implements IBarrel {
  clone(): this { return this; }     // OK
}

class BadTwoStep implements IBarrel {
  clone(): IBarrel { return this; } // ERROR — must return `this`, not `IBarrel`
}
