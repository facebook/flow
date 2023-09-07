declare export opaque type OpaqueType1;
export opaque type OpaqueType2 = string;
declare export opaque type OpaqueType3: string; // error: string ~> number
declare export opaque type OpaqueType4: string; // error: mixed ~> string (invariant subtyping)
declare export opaque type OpaqueType5; // error: mixed ~> number

declare export const o1: OpaqueType1;
declare export const o2: OpaqueType2;
declare export const o3: OpaqueType3; // error: string ~> number
declare export const o4: OpaqueType4; // error: mixed ~> string (invariant subtyping)
declare export const o5: OpaqueType5; // error: mixed ~> number

declare export opaque type GenericOpaqueType1<T1>;
declare export opaque type GenericOpaqueType2<T1>;
declare export opaque type GenericOpaqueType3<T1, T2>; // should error, but misplaced in opaque_type.js.flow
declare export opaque type GenericOpaqueType4<T1>; // should error, but misplaced in opaque_type.js.flow
declare export opaque type GenericOpaqueType5<T1, T2>: [T1, T2];
declare export opaque type GenericOpaqueType6<T1, T2>: [T2, T2]; // error

declare export const go1: GenericOpaqueType1<string>;
declare export const go2: GenericOpaqueType2<string>;
declare export const go3: GenericOpaqueType3<string, string>; // error
declare export const go4: GenericOpaqueType4<string>; // error
declare export const go5: GenericOpaqueType5<string, number>;
declare export const go6: GenericOpaqueType5<number, string>; // error: non-matching targs
