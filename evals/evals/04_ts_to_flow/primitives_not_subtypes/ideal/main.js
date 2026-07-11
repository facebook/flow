/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

function longestLine(lines: Iterable<string>): number {
  let best = 0;
  for (const line of lines) {
    if (line.length > best) {
      best = line.length;
    }
  }
  return best;
}

const changelog = [
  'Fix crash on empty input',
  'Update dependency versions',
  'Ship native module',
];
console.log('changelog max =', longestLine(changelog));

const commitTitle = 'Ship native module';
console.log('commit max =', longestLine(Array.from(commitTitle)));
