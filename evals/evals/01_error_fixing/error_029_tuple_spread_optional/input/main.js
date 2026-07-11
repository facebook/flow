/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

function nameRow(middle: [middle?: string]): [string, string, string] {
  return ['Ada', ...middle, 'Lovelace'];
}

console.log(nameRow(['Countess']).join(' '));
console.log(nameRow([]).join(' '));
