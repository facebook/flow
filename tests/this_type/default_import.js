// Bug A regression: default-imported class-like binding must rebind
// polymorphic `this` through `interface Child extends DefaultI` and
// `class Child extends DefaultC` consumers across module boundaries.

import type CDefault from './default_export';
import CDefaultV from './default_export';

// extends-via-interface — sig-merge path goes through
// `import_default_for_extends` -> `ImportDefaultTKit.on_ModuleT_for_extends`,
// skipping `canonicalize_imported_type`'s `ClassT(ThisInstanceT _)` unwrap.
interface IExtChild extends CDefault {
  extra: string;
}
declare const ec: IExtChild;
ec.clone() as IExtChild; // OK — polymorphic `this` rebound to IExtChild
ec.clone() as CDefault; // OK — IExtChild is also a CDefault

// extends-via-class — statement-typing path goes through
// `binding_kind_of_generic_id_post_convert`'s `Name_def.Default` arm,
// which calls `import_default_specifier_type_for_extends`.
class GoodCloneSub extends CDefaultV {
  clone(): this { return this; }
}

class BadCloneSub extends CDefaultV {
  clone(): CDefaultV { return this; } // ERROR — must return `this`, not `CDefaultV`
}

declare const gcs: GoodCloneSub;
gcs.clone() as GoodCloneSub; // OK
