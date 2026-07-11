/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Status =
  | {type: 'loading'}
  | {type: 'error', message: string}
  | {type: 'success', data: {count: number, items: Array<string>}};

export function renderStatus(status: Status): string {
  return match (status) {
    {type: 'loading'} => 'Loading...',
    {type: 'error', message: const msg} => `Error: ${msg}`,
    {type: 'success', data: {count: const c, const items}} =>
      `Found ${c} items: ${items.join(', ')}`,
  };
}
