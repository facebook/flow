/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

declare function persist(key: string, value: string): Promise<void>;

export async function saveAll(entries: Array<[string, string]>): Promise<void> {
  for (const [key, value] of entries) {
    await persist(key, value);
  }
}
