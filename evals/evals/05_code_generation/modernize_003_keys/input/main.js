/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Permissions = {
  read: boolean,
  write: boolean,
  admin: boolean,
};

type PermissionName = $Keys<Permissions>;

export function isGranted(perms: Permissions, name: PermissionName): boolean {
  return perms[name];
}
