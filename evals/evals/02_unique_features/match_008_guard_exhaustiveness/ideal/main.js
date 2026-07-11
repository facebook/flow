/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Item = {name: string, price: number, category: 'food' | 'electronics' | 'clothing'};

export function discount(item: Item): number {
  return match (item) {
    {category: 'food', const price, ...} if (price > 50) => price * 0.9,
    {category: 'electronics', const price, ...} if (price > 100) => price * 0.85,
    {category: 'clothing', const price, ...} if (price > 75) => price * 0.8,
    {const price, ...} => price,
  };
}
