/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Step = -1 | 0 | 1;

export function move(position: number, step: Step): number {
  return match (step) {
    -1 => position - 8,
    0 => position,
    1 => position + 8,
  };
}
