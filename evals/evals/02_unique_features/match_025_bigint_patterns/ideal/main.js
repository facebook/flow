/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export function accountLabel(id: bigint): string {
  return match (id) {
    0n => 'system',
    1n => 'root',
    2n => 'service',
    const other => `account #${String(other)}`,
  };
}
