/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import type {UserId} from 'UserId';

export function buildProfileUrl(id: UserId): string {
  return 'https://internal/profile/' + id;
}

export function sortIds(ids: ReadonlyArray<UserId>): Array<UserId> {
  return [...ids].sort((a, b) => a.localeCompare(b));
}
