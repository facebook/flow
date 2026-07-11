/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import Status from 'Status';

export type {Status};

export function isActive(status: Status): boolean {
  return status === Status.Active;
}

export function parseStatus(input: string): Status | void {
  return Status.cast(input);
}
