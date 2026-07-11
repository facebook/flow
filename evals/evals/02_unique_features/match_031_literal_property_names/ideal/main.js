/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type CacheEntry = {
  'content-type': 'json' | 'html',
  'max-age': number,
};

export function cachePolicy(entry: CacheEntry): string {
  return match (entry) {
    {'content-type': 'json', 'max-age': const age} => `cache json for ${age}s`,
    {'content-type': 'html', 'max-age': const age} => `cache html for ${age}s`,
  };
}
