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

// TODO: Implement
