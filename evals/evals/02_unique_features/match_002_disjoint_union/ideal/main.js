/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Action =
  | {type: 'increment', amount: number}
  | {type: 'decrement', amount: number}
  | {type: 'reset'}
  | {type: 'set', value: number};

export function processAction(count: number, action: Action): number {
  return match (action) {
    {type: 'increment', amount: const amt} => count + amt,
    {type: 'decrement', amount: const amt} => count - amt,
    {type: 'reset'} => 0,
    {type: 'set', value: const v} => v,
  };
}

