/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type RequestArgs = [string, number];

type PendingArgs = {[K in keyof RequestArgs]: Promise<RequestArgs[K]>};

export async function awaitUrl(pending: PendingArgs): Promise<string> {
  return await pending[0];
}
