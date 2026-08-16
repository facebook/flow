/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// The `unique symbol` value type of each property or class field carries that
// member's name, so a key derived from it (cross-module) renders as `[name]`
// rather than `[symbol]`.
export declare const Keys: {
  readonly a: unique symbol,
  readonly b: unique symbol,
};

export declare class D {
  static readonly dsk: unique symbol;
}

export interface I {
  readonly ifk: unique symbol;
}
