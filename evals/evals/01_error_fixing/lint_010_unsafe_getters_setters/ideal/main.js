/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

let store = 0;

export const counter = {
  value(): number {
    return store;
  },
  increment(): void {
    store += 1;
  },
};

export function readCount(): number {
  return counter.value();
}
