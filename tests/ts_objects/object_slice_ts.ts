/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import {
  type FlowDog,
  type FlowNarrowValue,
  flowNarrowValue,
} from "./object_slice_flow_base";

export type TsMarker = {marker: string};

interface TsBaseInterface {
  inherited: FlowDog;
  inheritedFunction: () => string;
  inheritedMethod(): string;
}

export interface TsNarrowInterface extends TsBaseInterface {
  kind: "ts";
  value: FlowDog;
  extra: string;
  ownFunction: () => string;
  ownMethod(): string;
}

export declare const tsNarrowInterfaceValue: TsNarrowInterface;

export type TsDefaults = {marker: string};
export type TsNarrowValue = {value: FlowDog; extra: string};

export declare const tsNarrowValue: TsNarrowValue;

export type TsTypeSpreadOfFlow = {...FlowNarrowValue}; // OK
export const tsValueSpreadOfFlow = {...flowNarrowValue}; // OK
