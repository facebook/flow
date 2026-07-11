/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type ExactColor = {
  r: number,
  g: number,
  b: number,
};

type LooseColor = {...ExactColor, ...};

export function toHex(color: ExactColor): string {
  return '#' + [color.r, color.g, color.b].map(c => c.toString(16)).join('');
}

export function withAlpha(): LooseColor {
  return {r: 0, g: 0, b: 0, a: 1};
}
