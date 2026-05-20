// Check that imports are handled properly with this types

import { A1 } from './export';
import type { A2 } from './export';
import { A3 } from './export';

class B1 extends A1 {
  foo(): B1 { return new B1(); } // ERROR
}

new B1().bar() as B1; // OK

class B3<X> extends A3<X> {
  foo(): B3<X> { return new B3(); } // ERROR
}

new B3<unknown>().bar() as B3<any>; // OK
new B3<string>().qux(0) as string; // ERROR

new B3<unknown>().bar() as A2<any>; // OK
new B3<string>().bar() as B3<string> as A2<number>; // ERROR
(new B3() as A2<number>).qux(0) as string; // ERROR

import Export from './export';

declare const a4: Export.A4;
let _ = a4.foo();

// Interface imports — verify the type-sig path for `this` round-trips
// across module boundaries.
import type { IBuilder, IBox, IExtended, ICloneable } from './export';

declare const builder: IBuilder;
builder.clone() as IBuilder; // OK
builder.init() as Promise<IBuilder>; // OK
builder.on('x', () => {}) as IBuilder; // OK — fluent

// Generic interface — `this`
declare const ibox: IBox<number>;
ibox.value() as number; // OK
ibox.clone() as IBox<number>; // OK — `this` rebound to IBox<number>

// Class implementing imported interface: implements check substitutes the
// interface's `this` with the implementing class's `this`.
class MyClone implements ICloneable {
  clone(): this { return this; }
}

// Cross-module implements: subclass overriding `clone(): this` with a wider
// concrete subtype name should error — the substituted `this` from the
// imported `ICloneable` is the implementing class's `this`, NOT `ICloneable`.
class GoodCloneCM implements ICloneable {
  clone(): this { return this; }   // OK
}

class BadCloneCM implements ICloneable {
  clone(): ICloneable { return this; }  // ERROR — must return `this`, not `ICloneable`
}

// Inherited `this` rebinds to the extending interface
declare const ex: IExtended;
ex.clone() as ICloneable; // OK
ex.clone() as IExtended; // OK — inherited `this` rebound to IExtended

// Same rebinding through a type-alias parent across the type-sig path:
// `IExtendedViaAlias extends ICloneableAlias` (where `ICloneableAlias =
// ICloneable`) resolves the inherited `this` to the child, matching the
// in-file behavior in `interface.js`.
//
// KNOWN LIMITATION: The line below currently errors. Type-alias parents
// (where the immediate parent in `extends` is a `type X = Interface` rather
// than an interface directly) don't preserve the polymorphic `this` shape
// through the alias. See `interface.js:65` for the in-file equivalent of
// this gap.
import type { IExtendedViaAlias } from './export';
declare const exa: IExtendedViaAlias;
exa.clone() as ICloneable; // OK
exa.clone() as IExtendedViaAlias; // OK — inherited `this` rebound through the imported alias (KNOWN LIMITATION: errors)

// Generic child of generic parent: type-arg substitution and `this`
// rebinding compose through the type-sig path.
import type { IChildBox } from './export';
declare const icb: IChildBox<number>;
icb.clone() as IChildBox<number>; // OK
icb.value() as number; // OK

// Namespace-member extends inherited `this`
// `Pack.TyRef` case.
import * as M from './export';
export interface INSChild extends M.ICloneable {
  extra: string;
}
declare const nsc: INSChild;
nsc.clone() as INSChild; // OK — inherited `this` rebound across `M.ICloneable`
nsc.clone() as ICloneable; // OK — INSChild is also a subtype of ICloneable
const nsExtra: string = nsc.clone().extra; // OK — `extra` accessible after rebinding

// `extends` an imported intersection alias. Members from each
// branch must flow through. Missing-prop access should report at the
// access site with `prop-missing`, not at the extends clause.
import type { IChildOfIntersection } from './export';
declare const ici: IChildOfIntersection;
ici.a() as number;  // OK — inherited from IA via the intersection alias
ici.b() as string;  // OK — inherited from IB via the intersection alias
ici.c() as boolean; // OK — own member
ici.zzz; // ERROR: prop-missing at this line, not at the extends clause
