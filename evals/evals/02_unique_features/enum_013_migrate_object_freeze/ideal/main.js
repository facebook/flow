/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export default enum Priority {
  Low = 1,
  Medium = 2,
  High = 3,
}

export function parsePriority(input: number): Priority | void {
  return Priority.cast(input);
}
