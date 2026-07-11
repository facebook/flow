/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Job =
  | {kind: 'fetch', url: string}
  | {kind: 'wait', ms: number}
  | {kind: 'noop'};

declare function doFetch(url: string): Promise<string>;
declare function delay(ms: number): Promise<void>;

export async function runJob(job: Job): Promise<void> {
  match (job) {
    {kind: 'fetch', const url} => {
      await doFetch(url);
    }
    {kind: 'wait', const ms} => {
      await delay(ms);
    }
    {kind: 'noop'} => {}
  }
}
