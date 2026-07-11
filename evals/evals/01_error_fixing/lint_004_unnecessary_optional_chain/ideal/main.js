/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type User = {profile: {name: string}};

export function displayName(user: User): string {
  return user.profile.name;
}
