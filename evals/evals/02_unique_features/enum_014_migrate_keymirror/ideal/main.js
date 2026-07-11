/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export default enum Status {
  Active,
  Paused,
  Off,
}

export function statusLabel(status: Status): string {
  return match (status) {
    Status.Active => 'Active now',
    Status.Paused => 'Temporarily paused',
    Status.Off => 'Turned off',
  };
}
