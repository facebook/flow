/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type CreateUser = (name: string, age: number) => {
  id: number,
  name: string,
  age: number,
};

type User = ReturnType<CreateUser>;

export function userSummary(user: User): string {
  return user.name + ' (' + String(user.age) + ')';
}
