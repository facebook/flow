/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import type {PrivateType} from "./dependency";

type PrivateAlias = PrivateType;
const privateValue = 1;

declare global {
  type FromImport = PrivateType; // ERROR
  type FromAlias = PrivateAlias; // ERROR
  type FromValue = typeof privateValue; // ERROR
  type InlineImport = import("./dependency").PrivateType;

  export type ExportedFromGlobal = string; // ERROR

  declare module "nested" { // ERROR
    declare export const nestedValue: string;
  }

  const initialized: string = "value"; // ERROR
  const validGlobal: string;

  declare global { // ERROR
    const nestedGlobal: string;
  }
}
