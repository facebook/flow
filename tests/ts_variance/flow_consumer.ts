/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import type {
  FlowAnimal,
  FlowAnimalMethod,
  FlowBox,
  FlowDog,
  FlowDogMethod,
  FlowMutableValue,
  FlowNarrowValue,
  FlowOptionalValues,
  FlowPresentValue,
  FlowReadonlyValue,
  FlowWideValue,
} from "./flow_lib";

declare class TsAnimal {}
declare class TsDog extends TsAnimal {
  bark(): void;
}

declare const tsDogFlowBox: FlowBox<TsDog>;
const tsAnimalFlowBox: FlowBox<TsAnimal> = tsDogFlowBox; // ERROR: Flow generic with TypeScript arguments

declare class TsBox<T> {
  value: T;
}

declare const flowDogTsBox: TsBox<FlowDog>;
const flowAnimalTsBox: TsBox<FlowAnimal> = flowDogTsBox; // OK: TypeScript generic with Flow arguments

type TsAnimalMethod = {
  cb(x: FlowAnimal): void;
};
declare const flowDogMethod: FlowDogMethod;
const tsAnimalMethod: TsAnimalMethod = flowDogMethod; // OK: Flow method to TypeScript method

type TsDogMethod = {
  cb(x: FlowDog): void;
};
declare const tsDogMethod: TsDogMethod;
const flowAnimalMethod: FlowAnimalMethod = tsDogMethod; // OK: TypeScript method to Flow method

type TsMutableValue = {value: string};
declare const flowReadonlyValue: FlowReadonlyValue;
const tsMutableValue: TsMutableValue = flowReadonlyValue; // OK: Flow property to TypeScript property

type TsReadonlyValue = {readonly value: string};
declare const tsReadonlyValue: TsReadonlyValue;
const flowMutableValue: FlowMutableValue = tsReadonlyValue; // OK: TypeScript property to Flow property

type TsWideValue = {value: FlowAnimal};
declare const flowNarrowValue: FlowNarrowValue;
const tsWideValue: TsWideValue = flowNarrowValue; // OK: Flow property to TypeScript property

type TsNarrowValue = {value: FlowDog};
declare const tsNarrowValue: TsNarrowValue;
const flowWideValue: FlowWideValue = tsNarrowValue; // OK: TypeScript property to Flow property

type TsOptionalValues = {a?: FlowAnimal; b?: FlowAnimal};
declare const flowPresentValue: FlowPresentValue;
const tsOptionalValues: TsOptionalValues = flowPresentValue; // OK: Flow object to TypeScript object

type TsPresentValue = {a: FlowAnimal};
declare const tsPresentValue: TsPresentValue;
const flowOptionalValues: FlowOptionalValues = tsPresentValue; // OK: TypeScript object to Flow object
