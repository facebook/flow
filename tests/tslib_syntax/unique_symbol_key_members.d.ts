/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Distinct `unique symbol` keys must produce distinct property-map entries in
// interfaces and declare classes (member-expression computed keys, as used by
// the TS builtin libs, e.g. `[Symbol.iterator]` / `[Symbol.unscopables]`).

export declare const Keys: {
  readonly a: unique symbol,
  readonly b: unique symbol,
};

export interface I {
  [Keys.a]: number;
  [Keys.b]: string;
}

// Overloaded method on the same symbol key merges into an intersection rather
// than colliding with the other symbol key.
export interface WithOverload {
  [Keys.a](x: number): number;
  [Keys.a](x: string): string;
  [Keys.b]: boolean;
}

export declare class C {
  [Keys.a]: number;
  [Keys.b]: string;
}
