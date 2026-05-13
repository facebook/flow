// Two sibling cases proving method-syntax is bivariant in .ts while
// arrow-property syntax remains contravariant -- matching TS
// --strictFunctionTypes.
//
// Note: bivariance only kicks in when BOTH sides use method syntax.
// One-sided cases (Method on the supertype, Field/arrow on the
// subtype) take the existing contravariant path -- TS rejects those
// at the property-shape level too.

declare class Animal {
  name: string;
}
declare class Dog extends Animal {
  bark(): void;
}

// Method-syntax on both sides: bivariant in .ts, so a narrower
// parameter (Dog) on the source is accepted against Animal.
type MethodHolder = {
  cb(x: Animal): void;
};

const m: MethodHolder = {cb(x: Dog): void {}}; // OK: method-syntax bivariance

// Reverse direction: source method has WIDER param (Animal) than target
// (Dog). This is the safe contravariant direction; the original
// (pre-bivariance) Flow path accepted it. TS bivariance also accepts it.
// True bivariance must accept BOTH directions when the param types are
// related, not just one.
type DogMethodHolder = {
  cb(x: Dog): void;
};

declare const widerCb: {cb(x: Animal): void};
const m2: DogMethodHolder = widerCb; // OK: contravariant (safe) direction

// Arrow-property syntax on the supertype: still contravariant.
// Source is also arrow syntax to keep the comparison apples-to-apples.
type ArrowHolder = {
  cb: (x: Animal) => void;
};

const a: ArrowHolder = {cb: (x: Dog): void => {}}; // ERROR: contravariance

// Method bivariance must NOT throw out arity checks. A source method with
// extra required parameters cannot be safely called from a caller of the
// target type (it would be invoked with too few args). TS rejects this.
const tooManyParams: MethodHolder = {
  cb(x: Dog, y: number): void {}, // ERROR: missing arg `y` when called with the target's arity
};

// Generic methods (PolyT-wrapped FunT) must also get bivariance treatment.
// Without the PolyT case, a generic method on the source falls through to
// the strict path and the safe direction errors spuriously.
type GenericMethodHolder = {
  cb<T>(x: T, animal: Animal): T;
};

const g: GenericMethodHolder = {
  cb<U>(x: U, animal: Dog): U {
    return x;
  },
}; // OK: generic method, narrower param via bivariance

// Soundness: when the source method has a rest param and the target has an
// extra fixed param, the rest's element type must still be compatible with
// the extra. The walker now flows the extras into ft1.rest_param so this
// errors instead of silently accepting -- otherwise a caller of the target
// signature could pass a `number` that the source reads as a `string`.
type FixedTarget = {
  cb(x: Animal, y: number): void;
};
declare const restSource: {cb(x: Animal, ...ys: string[]): void};
const restMismatch: FixedTarget = restSource; // ERROR: number/string[] incompatible

// Rest-vs-rest with incompatible element types must still error. Bivariance
// permits either direction of element-level subtyping but not unrelated
// element types.
type RestTarget = {
  cb(...ys: string[]): void;
};
declare const restNumberSource: {cb(...ys: number[]): void};
const restRestMismatch: RestTarget = restNumberSource; // ERROR: bivariance still requires related types

// One-sided method/field shapes must still take the existing contravariant
// path -- bivariance only kicks in when BOTH sides use method syntax.
// Here the target uses method syntax but the source uses arrow-property
// (Field), so the safe contravariant direction is the only one accepted;
// the unsafe direction errors.
type MethodTarget = {
  cb(x: Animal): void;
};
const oneSidedOk: MethodTarget = {cb: (x: Animal): void => {}}; // OK: same param type, no variance to check
const oneSidedErr: MethodTarget = {cb: (x: Dog): void => {}}; // ERROR: arrow source, no bivariance
