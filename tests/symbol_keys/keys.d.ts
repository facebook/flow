/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Shared `unique symbol` declarations for the cross-module cases. A `.d.ts`
// body routes a bracketed member through the computed-key parser, so `[Keys.a]`
// names a symbol member of `I` and of `C` here.

export declare const Keys: {
  readonly a: unique symbol,
  readonly b: unique symbol,
};

export declare const s: unique symbol;
export declare const t: unique symbol;

// A `unique symbol` reached through a cross-module alias, so an indexer key
// written as `SKey` arrives at annotation inference behind an annotation rather
// than as a bare `unique symbol`.
export type SKey = typeof s;

export interface IndexedI {
  [k: SKey]: number;
}

export declare class IndexedC {
  [k: SKey]: number;
}

export interface I {
  [Keys.a]: number;
  [Keys.b]: string;
}

export declare class C {
  [Keys.a]: number;
  [Keys.b]: string;
}
