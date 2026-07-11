/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Success = {type: 'success', value: number};
type Failure = {type: 'failure', error: string};
type Result = Success | Failure;

export const collectValues = (results: Array<Result>): Array<number> =>
  results
    .filter(result => result.type === 'success')
    .map(result => result.value);
