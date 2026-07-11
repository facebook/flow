/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import keyMirror from 'keyMirror';

const Status = keyMirror({
  Active: null,
  Paused: null,
  Off: null,
});

export type StatusType = keyof typeof Status;

const STATUS_LABEL = {
  [Status.Active]: 'Active now',
  [Status.Paused]: 'Temporarily paused',
  [Status.Off]: 'Turned off',
};

export function statusLabel(status: StatusType): string {
  return STATUS_LABEL[status];
}

export default Status;
