declare export class Empty {} // ok

declare export class ImplHasExtra { // ok
  static bar: string;
  foo(): string;
}

declare export class ImplHasDifferentSig { // error
  static bar: string;
  foo(): string
}

declare class GoodDeepHierarchy1 {
  foo(): boolean;
  static s1(): boolean;
}
declare class GoodDeepHierarchy2 extends GoodDeepHierarchy1 {
  bar(): number;
  static s2(): number;
}
declare export class GoodDeepHierarchy extends GoodDeepHierarchy2 {
  baz(): string;
  static s3(): string;
}

declare class BadDeepHierarchy1 {
  foo(): number; /* boolean in interface file */
  static s1(): number; /* boolean in interface file */
}
declare class BadDeepHierarchy2 extends BadDeepHierarchy1 { // error
  bar(): string; /* number in interface file */
  static s2(): string; /* number in interface file */
}
declare export class BadDeepHierarchy extends BadDeepHierarchy2 { // error
  baz(): boolean; /* string in interface file */
  static s3(): boolean; /* string in interface file */
}

declare class SameShapeDifferentNominalHierarchy11 {
  foo(): boolean;
  static s1(): boolean;
}
declare class SameShapeDifferentNominalHierarchy22 extends SameShapeDifferentNominalHierarchy11 {
  bar(): number;
  static s2(): number;
}
declare export class SameShapeDifferentNominalHierarchy extends SameShapeDifferentNominalHierarchy22 { // error
  baz(): string;
  static s3(): string;
}

declare export class Generic<T1> {
  foo(): T1;
}

declare export class GenericVsNonGeneric<T1> { // error
  foo(): T1;
}

declare export class GenericSwitchedTparams<T1, T2> { // error
  foo(): T1;
  bar(): T2;
}

declare export class GenericDifferentSizedTypeParametersError1<T1, T2> { // error
  foo(): T1;
}

declare export class GenericDifferentSizedTypeParametersError2<T1> { // error
  foo(): T1;
}

declare export const generic1: Generic<string>; // error
declare export const generic2: GenericDifferentSizedTypeParametersError1<string, string>; // error
declare export const generic3: GenericDifferentSizedTypeParametersError2<string>; // error
