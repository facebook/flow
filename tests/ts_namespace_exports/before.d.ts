/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

type Local = number;
export declare namespace BeforeClass {
  export { type Local as Item };
}
export declare class BeforeClass {
  field: number;
}
export declare namespace BeforeFunction {
  export { type Local as Item };
}
export declare function BeforeFunction(s: string): number;
