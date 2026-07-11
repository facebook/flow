/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type RequestArgs = [string, number];

type PendingArgs = $TupleMap<RequestArgs, <V>(V) => Promise<V>>;

export async function awaitUrl(pending: PendingArgs): Promise<string> {
  return await pending[0];
}
