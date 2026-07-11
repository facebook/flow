/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type User = {
  name: string,
  email: string,
};

type Admin = {
  name: string,
  email: string,
  role: string,
};

function greetUser(user: User): string {
  return `Hello, ${user.name}!`;
}

function processUsers(): void {
  const admin: Admin = {name: 'Alice', email: 'alice@example.com', role: 'superadmin'};

  greetUser(admin);

  const users: Array<User> = [
    {name: 'Bob', email: 'bob@example.com'},
    {name: 'Carol', email: 'carol@example.com', age: 30},
  ];
}
