/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Two sibling cases proving method-syntax is bivariant in .ts while
// arrow-property syntax remains contravariant.
//
// Note: bivariance only kicks in when BOTH sides use method syntax.
// One-sided cases (Method on the supertype, Field/arrow on the
// subtype) take Flow's existing contravariant path. TS accepts the
// Method-target/arrow-source case, so that negative below intentionally
// guards Flow's current narrower relaxation.

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

type Listener<T> = (event: T) => void;

type BaseEventLike = {
  addEventListener(type: string, listener: Listener<Animal> | null): void;
};

declare const preciseEventLike: {
  addEventListener(type: "dog", listener: Listener<Dog>): void;
  addEventListener(type: string, listener: Listener<Animal>): void;
};

// Overloaded method-syntax on the source side: the string fallback overload
// covers the base EventTarget-like signature, and method bivariance accepts the
// missing `null` branch in the listener parameter. This mirrors the DOM
// EventTarget pattern in TypeScript's builtin libs.
const overloadedSource: BaseEventLike = preciseEventLike; // OK

type OverloadedEventLike = {
  addEventListener(type: "dog", listener: Listener<Dog> | null): void;
  addEventListener(type: string, listener: Listener<Animal> | null): void;
};

// Overloaded method-syntax on both sides: every target overload must be
// covered, but each target overload may be covered by any compatible source
// overload.
const overloadedBoth: OverloadedEventLike = preciseEventLike; // OK

interface BaseEventInterface {
  addEventListener(type: string, listener: Listener<Animal> | null): void;
}

interface PreciseEventInterface extends BaseEventInterface {
  addEventListener(type: "dog", listener: Listener<Dog>): void;
  addEventListener(type: string, listener: Listener<Animal>): void;
}

declare const wrongOverloadedEventLike: {
  addEventListener(type: 1, listener: Listener<Animal>): void;
  addEventListener(type: 2, listener: Listener<Dog>): void;
};

const overloadedWrongKey: BaseEventLike = wrongOverloadedEventLike; // ERROR: no overload accepts string events

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

interface BaseEventMap {
  base: Animal;
}

interface PreciseEventMap extends BaseEventMap {
  dog: Dog;
}

type KeyedBaseEventLike = {
  addEventListener<K extends keyof BaseEventMap>(type: K, listener: Listener<BaseEventMap[K]>): void;
  addEventListener(type: string, listener: Listener<Animal>): void;
};

declare const preciseKeyedEventLike: {
  addEventListener<K extends keyof PreciseEventMap>(
    type: K,
    listener: Listener<PreciseEventMap[K]>,
  ): void;
  addEventListener(type: string, listener: Listener<Animal>): void;
};

// `keyof` on TS interfaces includes inherited interface fields. This mirrors
// DOM event maps like `DocumentEventMap extends GlobalEventHandlersEventMap`.
const inheritedEventMapKeys: KeyedBaseEventLike = preciseKeyedEventLike; // OK

interface WindowishEventMap {
  afterprint: Animal;
}

interface ElementishEventMap {
  click: Animal;
}

interface BodyEventMap extends ElementishEventMap, WindowishEventMap {
}

interface WindowishHandlers {
  addEventListener<K extends keyof WindowishEventMap>(
    type: K,
    listener: (ev: WindowishEventMap[K]) => void,
  ): void;
}

// Multiple `extends` entries are stored through lazy interface references in
// the super chain. `keyof BodyEventMap` must force those references and include
// `afterprint` from `WindowishEventMap`.
interface BodyElement extends WindowishHandlers {
  addEventListener<K extends keyof BodyEventMap>(
    type: K,
    listener: (ev: BodyEventMap[K]) => void,
  ): void;
}

interface WindowishHandlersWithThis {
  addEventListener<K extends keyof WindowishEventMap>(
    type: K,
    listener: (this: WindowishHandlersWithThis, ev: WindowishEventMap[K]) => void,
  ): void;
}

interface BodyWithThisEventMap extends WindowishEventMap {
}

// DOM listener overloads also narrow the callback `this` context in subtypes.
// In TS method-bivariant parameter checking, that callback `this` must not
// recursively make `BodyWithThisElement` fail against its parent listener.
interface BodyWithThisElement extends WindowishHandlersWithThis {
  bodyOnly: Dog;
  addEventListener<K extends keyof BodyWithThisEventMap>(
    type: K,
    listener: (this: BodyWithThisElement, ev: BodyWithThisEventMap[K]) => void,
  ): void;
}
