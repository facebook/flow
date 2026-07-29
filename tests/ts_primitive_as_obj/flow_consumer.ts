/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import {type FlowHasLength, flowString} from "./flow_lib";

interface TsHasLength {
  length: number;
}

flowString satisfies TsHasLength; // OK: Flow value to TypeScript interface

declare const tsString: string;
tsString satisfies FlowHasLength; // ERROR: TypeScript value to Flow interface
