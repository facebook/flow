/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Columns = [string, number, boolean];

type IndexedColumns = {[K in keyof Columns]: [K, Columns[K]]};

export function secondValue(indexed: IndexedColumns): number {
  return indexed[1][1];
}
