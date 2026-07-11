/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

function statusBadge(level: 'ok' | 'warn' | 'error', code: number): number | string | boolean {
  let badge: number | string | boolean;
  if (level === 'ok') {
    badge = code;
  } else {
    badge = level.toUpperCase();
  }
  if (code === 0) {
    badge = false;
  }
  return badge;
}

statusBadge('ok', 200);
