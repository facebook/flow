/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare class TsAnimal {}
declare class TsDog extends TsAnimal {
  bark(): void;
}

declare class TsBuiltinBox<T> {
  value: T;
}

declare var flowDogTsBox: TsBuiltinBox<FlowDog>;
declare var tsDogFlowBox: FlowBuiltinBox<TsDog>;

interface TsBaseMap {
  mousedown: number;
}
interface TsDerivedMap extends TsBaseMap {
  custom: string;
}
declare var tsKeys: keyof TsDerivedMap;

interface TsListener {
  on<K extends keyof TsDerivedMap>(type: K, value: TsDerivedMap[K]): void;
  on(type: string, value: string): void;
}
declare var tsListener: TsListener;
