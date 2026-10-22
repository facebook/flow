/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare const outerConst: number;
declare let outerLet: number;

export declare class ReadonlyClass {}
declare namespace ReadonlyClass {
  export declare const value: number;
  export declare let mutable: number;
  export declare var variable: number;
  export { outerConst as alias, outerLet as mutableAlias };
}

export declare function ReadonlyFunction(): void;
declare namespace ReadonlyFunction {
  export declare const value: number;
  export declare let mutable: number;
  export declare var variable: number;
  export { outerConst as alias, outerLet as mutableAlias };
}
