/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type MathOp = '+' | '-' | '*' | '/' | '%';

export function calculate(a: number, b: number, op?: MathOp): number {
  return match (op) {
    '+' | undefined => a + b,
    '-' => a - b,
    '*' => a * b,
    '/' => a / b,
    '%' => a % b,
  };
}
