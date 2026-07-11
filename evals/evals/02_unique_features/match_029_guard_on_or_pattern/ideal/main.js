/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export function shouldRetry(status: number, networkUp: boolean): boolean {
  return match (status) {
    502 | 503 | 504 if (networkUp) => true,
    _ => false,
  };
}
