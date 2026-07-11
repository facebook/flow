/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export function summarize(
  scores: $ReadOnlyMap<string, number>,
  tags: $ReadOnlySet<string>,
  cache: $ReadOnlyWeakMap<{id: string}, number>,
  visited: $ReadOnlyWeakSet<{id: string}>,
  key: string,
  node: {id: string},
): number {
  const base = scores.get(key) ?? 0;
  const bonus = tags.has(key) ? 10 : 0;
  const cached = cache.get(node) ?? 0;
  const seen = visited.has(node) ? 1 : 0;
  return base + bonus + cached + seen;
}
