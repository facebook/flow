/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import invariant from 'invariant';

type Token =
  | {kind: 'number', value: number}
  | {kind: 'string', value: string}
  | {kind: 'eof'}
  | {kind: 'error', msg: string};

export function processToken(token: Token): string {
  return match (token) {
    {kind: 'number', value: const v} => `Num: ${v}`,
    {kind: 'string', value: const v} => `Str: ${v}`,
    {kind: 'eof'} => 'End',
    {kind: 'error', msg: const m} => invariant(false, `Unexpected error token: ${m}`),
  };
}
