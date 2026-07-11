/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Address = {
  city: string,
  zip?: string,
};

type User = {
  name: string,
  address?: ?Address,
};

function getZip(user: ?User): string {
  return user?.address?.zip ?? '00000';
}

function formatUser(user: ?User): string {
  if (user == null) {
    return 'anonymous';
  }
  const city = user.address?.city ?? 'unknown';
  return `${user.name} (${city})`;
}

class Cache<V> {
  store: Map<string, V> = new Map<string, V>();

  get(key: string): V | void {
    return this.store.get(key);
  }

  getOrDefault(key: string, fallback: V): V {
    const value = this.store.get(key);
    return value !== undefined ? value : fallback;
  }

  set(key: string, value: V): void {
    this.store.set(key, value);
  }
}

const user: User = {name: 'Alice', address: {city: 'NYC'}};
const cache: Cache<number> = new Cache<number>();
cache.set('a', 1);

console.log(getZip(user), formatUser(null), cache.getOrDefault('b', 0));
