/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type BaseEntity = {
  id: string,
  createdAt: string,
};

type User = {
  ...BaseEntity,
  name: string,
  email?: string,
  roles: Array<string>,
};

type UserKeys = keyof User;
type UserUpdate = Partial<Omit<User, 'id' | 'createdAt'>>;
type UserSummary = Pick<User, 'id' | 'name'>;
type ReadonlyUser = Readonly<User>;
type UsersById = Record<string, User>;
type Email = NonNullable<User['email']>;

declare function loadUser(id: string): Promise<User>;
type LoadUserReturn = ReturnType<typeof loadUser>;
type LoadUserParams = Parameters<typeof loadUser>;

class UserStore {
  users: UsersById = {};

  add(user: User): void {
    this.users[user.id] = user;
  }

  get(id: string): ReadonlyUser | null {
    return this.users[id] ?? null;
  }

  update(id: string, patch: UserUpdate): User | null {
    const existing = this.users[id];
    if (existing == null) {
      return null;
    }
    const updated: User = {...existing, ...patch};
    this.users[id] = updated;
    return updated;
  }

  summaries(): Array<UserSummary> {
    return Object.values(this.users).map((u) => ({id: u.id, name: u.name}));
  }

  keys(): Array<UserKeys> {
    return ['id', 'createdAt', 'name', 'email', 'roles'];
  }

  requireEmail(id: string): Email {
    const u = this.users[id];
    if (u == null || u.email == null) {
      throw new Error('no email');
    }
    return u.email;
  }
}

const store = new UserStore();
store.add({
  id: '1',
  createdAt: 'now',
  name: 'Alice',
  email: 'alice@example.com',
  roles: ['admin'],
});
store.update('1', {name: 'Alice B.'});
console.log(store.summaries(), store.keys(), store.requireEmail('1'));
