/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

const click: [string, number, number, number] = ['click', 100, 5, 9];
const keydown: [string, number, string] = ['keydown', 200, 'Enter'];
const tick: [string, number] = ['tick', 300];

function describe(event: [string, number, ...]): string {
  return `${event[0]}@${event[1]}`;
}

export const summaries: Array<string> = [
  describe(click),
  describe(keydown),
  describe(tick),
];
