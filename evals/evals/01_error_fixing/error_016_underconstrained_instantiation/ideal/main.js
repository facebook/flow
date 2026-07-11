/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

function tally(words: ReadonlyArray<string>): Map<string, number> {
  const counts = new Map<string, number>();
  for (const word of words) {
    const prev = counts.get(word) ?? 0;
    counts.set(word, prev + 1);
  }
  return counts;
}

const histogram = tally(['apple', 'banana', 'apple']);
