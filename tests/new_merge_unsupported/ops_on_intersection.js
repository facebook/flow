// @flow

class C { static f: any; g: any }
class D { static f: any; g: any }

type T = typeof C & typeof D;
type S = { f: any } & { g: any };

declare var t: T;
declare var n: number & mixed;

// Annot_SpecializeT
export class x1 extends ((0: any): T) {}

// Annot_GetPropT
export const x2 = t.f;

// Annot_GetElemT
export const x3 = t[('': string)];

// Annot_ObjKitT Spread
export const x4 = { ...t };

// Annot_MakeExactT
declare var y5: $Exact<S>;
export const x5 = y5.f;

// Annot_GetStaticsT
declare var y6: Class<C & D>;
export const x6 = y6.f;

// Annot_LookupT
export class c7 extends ((0: any): T) {}
declare var y7: c7;
export const x7 = y7.g;

// Annot_ObjTestProtoT
declare export var x8: { __proto__: T };

// Annot_MixinT
declare export class x9 mixins t {}

// Annot_UnaryMinusT
export const x10 = -n;

// Annot_NotT
export const x11 = !n;

// Annot_ObjKeyMirror
declare var y12: $KeyMirror<S>;
export const x12 = y12.f;

// Annot_ObjMapConst
declare var y13: $ObjMapConst<S, any>;
export const x13 = y13.f;

// Annot_GetKeysT of Reason.t
declare var y14: $Keys<T>;
export const x14 = y14.length;
