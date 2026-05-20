// Bug B' regression: re-exported interface through a barrel module accessed
// via [import * as NS from './barrel'] must preserve polymorphic `this` so
// `extends`/`implements` consumers can rebind across the re-export chain
// even when the indirection goes through a synthesized namespace.
//
// The fix is in [ImportModuleNsTKit.on_ModuleT] in [flow_js_utils.ml]: the
// [named_symbol_to_field] helper must prefer [type_for_extends] (the raw,
// un-canonicalized exporter-side type) when present, so the synthesized
// namespace's [types_tmap] / values object carries the polymorphic-`this`
// shape that downstream [extends NS.X] / [implements NS.X] consumers need.

import * as NS from './barrel';

// extends-via-interface — qualified id resolves via
// [Annot_GetTypeFromNamespaceT] which reads the namespace's [types_tmap].
// With the fix, [types_tmap] carries the raw [type_for_extends] when present.
interface ChildOfNs extends NS.IBarrel {
  extra: string;
}
declare const cn: ChildOfNs;
cn.clone() as ChildOfNs; // OK — `this` rebound across the namespace barrel
cn.clone() as NS.IBarrel; // OK — ChildOfNs is also an NS.IBarrel

// Negative test: a sibling extender should be a separate `this`-rebinding,
// so its `clone()` must not be assignable to `ChildOfNs`.
interface SiblingOfNs extends NS.IBarrel {
  sibling: number;
}
declare const sn: SiblingOfNs;
sn.clone() as ChildOfNs; // ERROR — sibling `this` doesn't rebind to ChildOfNs
