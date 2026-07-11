/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

function nameRow(
  middle: [middle?: string],
): [string, string] | [string, string, string] {
  const [m] = middle;
  return m != null ? ['Ada', m, 'Lovelace'] : ['Ada', 'Lovelace'];
}

console.log(nameRow(['Countess']).join(' '));
console.log(nameRow([]).join(' '));
