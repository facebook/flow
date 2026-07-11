/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

declare function getStock(id: string): ?number;

export function stockLabel(id: string): string {
  const count = getStock(id);
  if (count != null) {
    return count + ' left';
  }
  return 'unavailable';
}
