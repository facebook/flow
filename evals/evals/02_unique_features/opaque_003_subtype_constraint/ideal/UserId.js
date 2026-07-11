/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export opaque type UserId: string = string;

export function toUserId(raw: string): UserId | null {
  return /^[0-9]+$/.test(raw) ? raw : null;
}
