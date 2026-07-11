/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Quadrant = 'NE' | 'NW' | 'SE' | 'SW';

export function offset(q: Quadrant, distance: number): string {
  const [dx, dy] = match (q) {
    'NE' => [1, -1],
    'NW' => [-1, -1],
    'SE' => [1, 1],
    'SW' => [-1, 1],
  };
  return `dx=${dx * distance}, dy=${dy * distance}`;
}
