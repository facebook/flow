const React = require('react');

type Props = {f?: number, g: number};
type DefaultProps = {f: number};

type T = {f?: number, g: number} extends (infer O) ? O : empty;
declare const t: T;

// Annot_SpecializeT
export class x1 extends (0 as any as T) {}

// Annot_GetPropT
export const x2 = t.f;

// Annot_GetElemT
export const x3 = t['' as string];

// Annot_ObjKitT Spread
export const x4 = { ...t };

// Annot_MakeExactT
declare const y5: $Exact<T>;
export const x5 = y5.f;

// Annot_GetStaticsT
declare const y6: Class<T>;
export const x6 = y6.f;

// Annot_LookupT
export class c7 extends (0 as any as Class<T>) {}
declare const y7: c7;
export const x7 = y7.f;

// Annot_ObjTestProtoT
declare export const x8: { __proto__: T };

// Annot_MixinT
declare export class x9 mixins t {}

// Annot_UnaryMinusT
export const x10 = -t;

// Annot_NotT
export const x11 = !t;

// Annot_ObjKeyMirror
declare const y12: $KeyMirror<T>;
export const x12 = y12.f;

// Annot_GetKeysT of Reason.t
declare const y13: keyof T;
export const x13 = y13.length;
