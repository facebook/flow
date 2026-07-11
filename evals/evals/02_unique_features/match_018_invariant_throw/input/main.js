/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import invariant from './invariant';

type Token =
  | {kind: 'number', value: number}
  | {kind: 'string', value: string}
  | {kind: 'eof'}
  | {kind: 'error', msg: string};

// TODO: Implement
