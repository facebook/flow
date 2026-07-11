/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum Permission {
  Read,
  Write,
  Execute,
  Admin,
}

export function parsePermission(input: string): Permission {
  return Permission.cast(input) ?? Permission.Read;
}

export function canModify(perm: Permission): boolean {
  switch (perm) {
    case Permission.Write:
    case Permission.Admin:
      return true;
    case Permission.Read:
    case Permission.Execute:
      return false;
  }
}

export function describeAccess(perm: Permission): string {
  switch (perm) {
    case Permission.Read:
      return 'view only';
    case Permission.Write:
      return 'can edit';
    case Permission.Execute:
      return 'can run';
    case Permission.Admin:
      return 'full control';
  }
}
