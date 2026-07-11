/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

interface Cache {
  get(key: string): string | void;
  set(key: string, value: string): boolean;
}

class MemoryCache {
  #store: Map<string, string> = new Map();

  get(key: string): string | void {
    return this.#store.get(key);
  }

  set(key: string, value: string): boolean {
    const isNew = !this.#store.has(key);
    this.#store.set(key, value);
    return isNew;
  }
}

class LruCache {
  #store: Map<string, string> = new Map();
  #capacity: number;

  constructor(capacity: number) {
    this.#capacity = capacity;
  }

  get(key: string): string | void {
    const value = this.#store.get(key);
    if (value !== undefined) {
      this.#store.delete(key);
      this.#store.set(key, value);
    }
    return value;
  }

  set(key: string, value: string): boolean {
    const isNew = !this.#store.has(key);
    this.#store.delete(key);
    this.#store.set(key, value);
    while (this.#store.size > this.#capacity) {
      const oldest = this.#store.keys().next().value;
      if (oldest === undefined) {
        break;
      }
      this.#store.delete(oldest);
    }
    return isNew;
  }
}

function warmUp(
  cache: Cache,
  entries: ReadonlyArray<[string, string]>,
): number {
  let inserted = 0;
  for (const [key, value] of entries) {
    if (cache.set(key, value)) {
      inserted += 1;
    }
  }
  return inserted;
}

const entries: ReadonlyArray<[string, string]> = [
  ['user:1', 'alice'],
  ['user:2', 'bob'],
  ['user:1', 'alice'],
  ['user:3', 'carol'],
];

const memory: MemoryCache = new MemoryCache();
const lru: LruCache = new LruCache(2);

const memoryInserts: number = warmUp(memory, entries);
const lruInserts: number = warmUp(lru, entries);

console.log(`memory inserts: ${memoryInserts}`);
console.log(`lru inserts: ${lruInserts}`);
console.log(`memory has user:1 -> ${memory.get('user:1') ?? 'miss'}`);
console.log(`lru has user:1 -> ${lru.get('user:1') ?? 'miss'}`);
