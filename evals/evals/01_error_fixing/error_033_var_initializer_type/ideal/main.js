/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

function resolveTimeout(raw: number): number | string {
  let timeout: number | string = raw;
  if (raw <= 0) {
    timeout = 'unlimited';
  }
  return timeout;
}

resolveTimeout(30);
