/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export function fullLabel(this: {first: string, last: string}): string {
  return this.first + ' ' + this.last;
}
