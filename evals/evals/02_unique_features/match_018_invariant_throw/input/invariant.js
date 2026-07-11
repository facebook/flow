/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export default function invariant(
  condition: boolean,
  message: string,
): empty {
  if (!condition) {
    throw new Error(message);
  }
  throw new Error('invariant: condition was unexpectedly true');
}
