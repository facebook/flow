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
