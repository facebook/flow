type T = {[_K in keyof {f: number}]: string};
declare const t: T;

export const getProp = t.f;
export const getElem = t['f'];
export const spread = {...t};
declare const exact: $Exact<T>;
export const exactProp = exact.f;
declare export const proto: {__proto__: T};
export const not = !t;
declare const keyMirror: $KeyMirror<T>;
export const keyMirrorProp = keyMirror.f;
declare const keys: keyof T;
export const keysLength = keys.length;

type NonHomomorphic = {[K in 'a' | 'b']: number};
declare const nonHomomorphic: NonHomomorphic;
export const nonHomomorphicProp = nonHomomorphic.a;

type Optional = {[K in keyof {f: number}]?: string};
declare const optional: Optional;
export const optionalProp = optional.f;
