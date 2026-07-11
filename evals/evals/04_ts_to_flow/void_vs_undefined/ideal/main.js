/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type User = {id: number, name: string};

function findUser(users: Array<User>, id: number): User | void {
  return users.find(u => u.id === id);
}

function greet(user: User | void): string {
  if (user === undefined) {
    return 'Guest';
  }
  return `Hello, ${user.name}`;
}

const users: Array<User> = [
  {id: 1, name: 'Ada'},
  {id: 2, name: 'Bo'},
];

console.log(greet(findUser(users, 2)));
console.log(greet(findUser(users, 9)));
