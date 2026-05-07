type T = {[_K in keyof { f: number }]: string};
declare const t: T;

// Annot_GetPropT
export const x2 = t.f;

// Annot_GetElemT
export const x3 = t['' as string]; // error: unsafe string key access

// Annot_ObjKitT Spread
export const x4 = { ...t };

// Annot_MakeExactT
declare const y5: $Exact<T>;
export const x5 = y5.f;

// Annot_ObjTestProtoT
declare export const x8: { __proto__: T };

// Annot_NotT
export const x11 = !t;

// Annot_ObjKeyMirror
declare const y12: $KeyMirror<T>;
export const x12 = y12.f;

// Annot_GetKeysT of Reason.t
declare const y13: keyof T;
export const x13 = y13.length;
