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

export interface TsNarrowInterface {
  value: FlowDog;
  extra: string;
}

export type TsDefaults = {marker: string};
export type TsNarrowValue = {value: FlowDog; extra: string};

export declare const tsNarrowValue: TsNarrowValue;

export type TsTypeSpreadOfFlow = {...FlowNarrowValue}; // OK
export const tsValueSpreadOfFlow = {...flowNarrowValue}; // OK
