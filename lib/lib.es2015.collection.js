/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Modifications Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the
 * License at http://www.apache.org/licenses/LICENSE-2.0
 * THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION ANY IMPLIED
 * WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR A PARTICULAR PURPOSE,
 * MERCHANTABILITY OR NON-INFRINGEMENT.
 * See the Apache Version 2.0 License for specific language governing permissions
 * and limitations under the License.
 *
 * @flow
 */
// @lint-ignore-every LICENSELINT
/* Maps and Sets */

declare class $ReadOnlyMap<K, out V> {
    @@iterator(
      // $FlowFixMe[incompatible-variance]
    ): Iterator<[K, V]>;
    /**
     * Returns an iterable of key, value pairs for every entry in the map.
     */
    entries(
      // $FlowFixMe[incompatible-variance]
    ): Iterator<[K, V]>;
    forEach<This>(callbackfn: (this : This, value: V, index: K, map: ReadonlyMap<K, V>) => unknown, thisArg: This): void;
    get(key: K): V | void;
    has(key: K): boolean;
    /**
     * Returns an iterable of keys in the map
     */
    keys(): Iterator<K>;
    size: number;
    /**
     * Returns an iterable of values in the map
     */
    values(): Iterator<V>;
    readonly [Symbol.toStringTag]: 'Map';
}

declare class Map<K, V> extends $ReadOnlyMap<K, V> {
    @@iterator(): Iterator<[K, V]>;
    constructor(iterable?: ?Iterable<[readonly key: K, readonly value: V]>): void;
    clear(): void;
    delete(key: K): boolean;
    /**
     * Returns an iterable of key, value pairs for every entry in the map.
     */
    entries(): Iterator<[K, V]>;
    forEach<This>(callbackfn: (this : This, value: V, index: K, map: Map<K, V>) => unknown, thisArg: This): void;
    get(key: K): V | void;
    getOrInsert(key: K, value: V): V;
    getOrInsertComputed(key: K, callbackfn: (key: K) => V): V;
    has(key: K): boolean;
    /**
     * Returns an iterable of keys in the map
     */
    keys(): Iterator<K>;
    set(key: K, value: V): Map<K, V>;
    size: number;
    /**
     * Returns an iterable of values in the map
     */
    values(): Iterator<V>;
    static readonly [Symbol.species]: any;
    /**
     * Groups members of an iterable according to the return value of the passed callback.
     * @param items An iterable.
     * @param keySelector A callback which will be invoked for each item in items.
     */
    static groupBy<T, K>(items: Iterable<T>, keySelector: (item: T, index: number) => K): Map<K, Array<T>>;
}

declare class WeakMap<K extends WeaklyReferenceable, V> extends $ReadOnlyWeakMap<K, V> {
    constructor(iterable?: ?Iterable<[readonly key: K, readonly value: V]>): void;
    delete(key: K): boolean;
    get(key: K): V | void;
    getOrInsert(key: K, value: V): V;
    getOrInsertComputed(key: K, callbackfn: (key: K) => V): V;
    has(key: K): boolean;
    set(key: K, value: V): WeakMap<K, V>;
}

declare class $ReadOnlySet<out T> {
    @@iterator(): Iterator<T>;
    /**
     * Returns an iterable of [v,v] pairs for every value `v` in the set.
     */
    entries(
      // $FlowFixMe[incompatible-variance]
    ): Iterator<[T, T]>;
    forEach<This>(callbackfn: (this: This, value: T, index: T, set: ReadonlySet<T>) => unknown, thisArg: This): void;
    has(
      // $FlowFixMe[incompatible-variance]
      value: T
    ): boolean;
    /**
     * Despite its name, returns an iterable of the values in the set,
     */
    keys(): Iterator<T>;
    size: number;
    /**
     * Returns an iterable of values in the set.
     */
    values(): Iterator<T>;
    /** Takes a `Set` and returns a new `Set` containing elements in this `Set` but not in the given `Set`. */
    difference(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>
      // $FlowFixMe[incompatible-variance]
    ): Set<T>;
    /** Takes a `Set` and returns a new `Set` containing elements in both this `Set` and the given `Set`. */
    intersection(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>
      // $FlowFixMe[incompatible-variance]
    ): Set<T>;
    /** Takes a `Set` and returns a `boolean` indicating if this `Set` has no elements in common with the given `Set`. */
    isDisjointFrom(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>): boolean;
    /** Takes a `Set` and returns a `boolean` indicating if all elements of this `Set` are in the given `Set`. */
    isSubsetOf(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>
    ): boolean;
    /** Takes a `Set` and returns a `boolean` indicating if all elements of the given `Set` are in this `Set`. */
    isSupersetOf(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>
    ): boolean;
    /** Returns a new `Set` containing elements which are in either this `Set` or the given `Set`, but not in both. */
    symmetricDifference(
      // $FlowFixMe[incompatible-variance]
      other: ReadonlySet<T>
      // $FlowFixMe[incompatible-variance]
    ): Set<T>;
    readonly [Symbol.toStringTag]: 'Set';
}

declare class Set<T> extends $ReadOnlySet<T> {
    @@iterator(): Iterator<T>;
    constructor(iterable?: ?Iterable<T>): void;
    add(value: T): Set<T>;
    clear(): void;
    delete(value: T): boolean;
    /**
     * Returns an iterable of [v,v] pairs for every value `v` in the set.
     */
    entries(): Iterator<[T, T]>;
    forEach<This>(callbackfn: (this: This, value: T, index: T, set: Set<T>) => unknown, thisArg: This): void;
    has(value: T): boolean;
    /**
     * Despite its name, returns an iterable of the values in the set,
     */
    keys(): Iterator<T>;
    size: number;
    /**
     * Returns an iterable of values in the set.
     */
    values(): Iterator<T>;
    static readonly [Symbol.species]: (...any) => any; // This would the Set constructor, can't think of a way to correctly type this
}

declare class WeakSet<T extends WeaklyReferenceable> extends $ReadOnlyWeakSet<T> {
    constructor(iterable?: ?Iterable<T>): void;
    add(value: T): WeakSet<T>;
    delete(value: T): boolean;
    has(value: T): boolean;
}


type ReadonlyMap<K, out V> = $ReadOnlyMap<K, V>;

type ReadonlySet<out V> = $ReadOnlySet<V>;
