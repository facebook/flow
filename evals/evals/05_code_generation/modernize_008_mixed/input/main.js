/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export function describeValue(value: mixed): string {
  if (typeof value === 'string') {
    return 'text(' + String(value.length) + ')';
  }
  if (typeof value === 'number') {
    return 'number(' + String(value) + ')';
  }
  if (Array.isArray(value)) {
    return 'array(' + String(value.length) + ')';
  }
  return 'unknown';
}
