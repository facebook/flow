/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import {
  type FlowBox,
  type FlowExactBox,
  type FlowExactEmpty,
  type FlowJustA,
  FlowPoint,
  flowExtra,
  flowFunction,
  flowPointShape,
} from "./flow_lib";

declare class TsBox {
  v: number;
}

declare const flowBox: FlowBox;
flowBox satisfies {v: number}; // OK: Flow instance to TypeScript object

declare const tsBox: TsBox;
tsBox satisfies FlowExactBox; // OK: TypeScript instance to Flow object

flowFunction satisfies {}; // OK: Flow function to TypeScript object

declare function tsFunction(): void;
tsFunction satisfies FlowExactEmpty; // OK: TypeScript function to Flow object

flowExtra satisfies {a: number}; // OK: Flow object to TypeScript object

declare const tsExtra: {a: number; b: string};
tsExtra satisfies FlowJustA; // OK: TypeScript object to Flow object

declare class TsPoint {
  x: number;
  y: number;
}

flowPointShape satisfies TsPoint; // OK: Flow object to TypeScript class

declare const tsPointShape: {x: number; y: number};
tsPointShape satisfies FlowPoint; // ERROR: a Flow class remains nominal
