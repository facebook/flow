/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export function formatName(name: ?{first: string, last: string}): string {
  return match (name) {
    {const first, const last} => `${first} ${last}`,
    null => 'Anonymous',
    undefined => 'Unknown',
  };
}
