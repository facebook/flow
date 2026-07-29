/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Pure TS: a class and an interface that a `.js` consumer will exercise
// against exact and inexact object targets.

export declare class Box {
  v: number;
}

export interface IBox {
  v: number;
}

export declare class Point {
  x: number;
  y: number;
}

export declare const pointShape: {x: number; y: number};
