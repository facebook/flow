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

interface $ReadOnlyMap<K, out V> {
    forEach<This>(callbackfn: (this : This, value: V, index: K, map: ReadonlyMap<K, V>) => unknown, thisArg: This): void;
    get(key: K): V | void;
    has(key: K): boolean;
    size: number;
}

interface Map<K, V> extends $ReadOnlyMap<K, V> {
    clear(): void;
    delete(key: K): boolean;
    forEach<This>(callbackfn: (this : This, value: V, index: K, map: Map<K, V>) => unknown, thisArg: This): void;
    get(key: K): V | void;
    getOrInsert(key: K, value: V): V;
    getOrInsertComputed(key: K, callbackfn: (key: K) => V): V;
    has(key: K): boolean;
    set(key: K, value: V): Map<K, V>;
    size: number;
}

interface MapConstructor {
    readonly prototype: Map<any, any>;
    new<K, V>(iterable?: ?Iterable<[readonly key: K, readonly value: V]>): Map<K, V>;
}

declare var Map: MapConstructor;

interface WeakMap<K extends WeaklyReferenceable, V> extends $ReadOnlyWeakMap<K, V> {
    delete(key: K): boolean;
    get(key: K): V | void;
    getOrInsert(key: K, value: V): V;
    getOrInsertComputed(key: K, callbackfn: (key: K) => V): V;
    has(key: K): boolean;
    set(key: K, value: V): WeakMap<K, V>;
}

interface WeakMapConstructor {
    readonly prototype: WeakMap<WeaklyReferenceable, any>;
    new<K extends WeaklyReferenceable, V>(iterable?: ?Iterable<[readonly key: K, readonly value: V]>): WeakMap<K, V>;
}

declare var WeakMap: WeakMapConstructor;

interface $ReadOnlySet<out T> {
    forEach<This>(callbackfn: (this: This, value: T, index: T, set: ReadonlySet<T>) => unknown, thisArg: This): void;
    has(
      // $FlowFixMe[incompatible-variance]
      value: T
    ): boolean;
    size: number;
}

interface Set<T> extends $ReadOnlySet<T> {
    add(value: T): Set<T>;
    clear(): void;
    delete(value: T): boolean;
    forEach<This>(callbackfn: (this: This, value: T, index: T, set: Set<T>) => unknown, thisArg: This): void;
    has(value: T): boolean;
    size: number;
}

interface SetConstructor {
    readonly prototype: Set<any>;
    new<T>(iterable?: ?Iterable<T>): Set<T>;
}

declare var Set: SetConstructor;

interface WeakSet<T extends WeaklyReferenceable> extends $ReadOnlyWeakSet<T> {
    add(value: T): WeakSet<T>;
    delete(value: T): boolean;
    has(value: T): boolean;
}

interface WeakSetConstructor {
    readonly prototype: WeakSet<WeaklyReferenceable>;
    new<T extends WeaklyReferenceable>(iterable?: ?Iterable<T>): WeakSet<T>;
}

declare var WeakSet: WeakSetConstructor;


interface ReadonlyMap<K, out V> extends $ReadOnlyMap<K, V> {}

interface ReadonlySet<out V> extends $ReadOnlySet<V> {}
