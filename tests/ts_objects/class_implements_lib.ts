/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// .ts library exporting an obj-typed shape; a `.js` consumer next door
// declares a class that implements it.
// The imported shape retains TypeScript strictness.

export type Shape = {a: number; b: string};
