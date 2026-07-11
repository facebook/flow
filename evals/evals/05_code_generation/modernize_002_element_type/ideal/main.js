/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Scores = Array<number>;

type Score = Scores[number];

export function isPassing(score: Score): boolean {
  return score >= 60;
}
