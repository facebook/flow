/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type Cache = {
  get(key: string): string | null,
  set(key: string, value: string): void,
  hitRate(): number,
};

class LruCache implements Cache {
  capacity: number;
  entries: Map<string, string> = new Map();
  hits: number = 0;
  misses: number = 0;

  constructor(capacity: number) {
    this.capacity = capacity;
  }

  get(key: string): string | null {
    const v = this.entries.get(key);
    if (v === undefined) {
      this.misses += 1;
      return null;
    }
    this.entries.delete(key);
    this.entries.set(key, v);
    this.hits += 1;
    return v;
  }

  set(key: string, value: string): void {
    if (this.entries.has(key)) {
      this.entries.delete(key);
    } else if (this.entries.size >= this.capacity) {
      const oldest = this.entries.keys().next().value;
      if (oldest !== undefined) {
        this.entries.delete(oldest);
      }
    }
    this.entries.set(key, value);
  }

  hitRate(): number {
    const total = this.hits + this.misses;
    return total === 0 ? 0 : this.hits / total;
  }
}

const cache: LruCache = new LruCache(2);
cache.set('a', '1');
cache.set('b', '2');
cache.get('a');
cache.get('c');
cache.set('d', '4');
console.log(`hit rate: ${cache.hitRate().toFixed(2)}, still has b: ${String(cache.get('b') !== null)}`);
