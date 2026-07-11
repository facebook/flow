/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

const Priority = Object.freeze({
  Low: 1,
  Medium: 2,
  High: 3,
});

export type PriorityType = Values<typeof Priority>;

export function parsePriority(input: number): PriorityType | void {
  switch (input) {
    case 1:
      return Priority.Low;
    case 2:
      return Priority.Medium;
    case 3:
      return Priority.High;
    default:
      return undefined;
  }
}

export default Priority;
