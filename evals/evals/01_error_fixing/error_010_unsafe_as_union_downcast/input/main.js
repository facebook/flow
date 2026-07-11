/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type Notification =
  | {kind: 'message', from: string, body: string}
  | {kind: 'reaction', from: string, emoji: string, count: number};

function describe(n: Notification): string {
  const message = n as {kind: 'message', from: string, body: string};
  return `${message.from}: ${message.body}`;
}

export {describe};
