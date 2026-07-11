/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type HAlign = 'left' | 'center' | 'right';
type VAlign = 'top' | 'bottom';

export function tooltipPosition(
  h: HAlign,
  v: VAlign,
): {x: number, y: number} {
  return match ([h, v]) {
    ['left', 'top'] => {x: -10, y: -10},
    ['left', 'bottom'] => {x: -10, y: 10},
    ['center', 'top'] => {x: 0, y: -10},
    ['center', 'bottom'] => {x: 0, y: 10},
    ['right', 'top'] => {x: 10, y: -10},
    ['right', 'bottom'] => {x: 10, y: 10},
  };
}
