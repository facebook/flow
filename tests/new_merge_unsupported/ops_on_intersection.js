class C { static f: any; g: any }
class D { static f: any; g: any }

type T = typeof C & typeof D;
type S = { f: any } & { g: any };

declare const t: T;
declare const n: number & unknown;

// Annot_SpecializeT
export class x1 extends (0 as any as T) {}

// Annot_GetPropT
export const x2 = t.f;

// Annot_GetElemT
export const x3 = t['' as string];

// Annot_ObjKitT Spread
export const x4 = { ...t };

// Annot_GetStaticsT
declare const y6: Class<C & D>;
export const x6 = y6.f;

// Annot_LookupT
export class c7 extends (0 as any as T) {}
declare const y7: c7;
export const x7 = y7.g;

// Annot_ObjTestProtoT
declare export const x8: { __proto__: T };

// Annot_MixinT
declare export class x9 mixins t {}

// Annot_UnaryMinusT
export const x10 = -n;

// Annot_NotT
export const x11 = !n;

// Annot_ObjKeyMirror
declare const y12: $KeyMirror<S>;
export const x12 = y12.f;

// Annot_GetKeysT of Reason.t
declare const y13: keyof T;
export const x13 = y13.length;
