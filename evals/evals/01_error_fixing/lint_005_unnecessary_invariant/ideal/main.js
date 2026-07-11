/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

declare function invariant(condition: boolean, message: string): void;

export function firstItem(items: Array<string>): string {
  invariant(items.length > 0, 'items must not be empty');
  return items[0];
}
