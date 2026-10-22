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

interface Map<K, V> extends ReadonlyMap<K, V> {
    clear(): void;
    delete(key: K): boolean;
    forEach<This>(callbackfn: (this: This, value: V, key: K, map: Map<K, V>) => unknown, thisArg: This): void;
    get(key: K): V | undefined;
    getOrInsert(key: K, value: V): V;
    getOrInsertComputed(key: K, callbackfn: (key: K) => V): V;
    has(key: K): boolean;
    set(key: K, value: V): this;
    size: number;
}

interface MapConstructor {
    new <K, V>(iterable?: ?Iterable<[readonly key: K, readonly value: V]>): Map<K, V>;
    readonly prototype: Map<any, any>;
}
declare var Map: MapConstructor;

interface ReadonlyMap<K, out V> {
    forEach<This>(callbackfn: (this: This, value: V, key: K, map: ReadonlyMap<K, V>) => unknown, thisArg: This): void;
    get(key: K): V | undefined;
    has(key: K): boolean;
    size: number;
}

type $ReadOnlyMap<K, out V> = ReadonlyMap<K, V>;

interface WeakMap<K extends WeakKey, V> extends ReadonlyWeakMap<K, V> {
    delete(key: K): boolean;
    get(key: K): V | undefined;
    getOrInsert(key: K, value: V): V;
    getOrInsertComputed(key: K, callbackfn: (key: K) => V): V;
    has(key: K): boolean;
    set(key: K, value: V): this;
}

interface WeakMapConstructor {
    new <K extends WeakKey, V>(iterable?: ?Iterable<[readonly key: K, readonly value: V]>): WeakMap<K, V>;
    readonly prototype: WeakMap<WeakKey, any>;
}
declare var WeakMap: WeakMapConstructor;

interface Set<T> extends ReadonlySet<T> {
    add(value: T): this;

    clear(): void;
    delete(value: T): boolean;
    forEach<This>(callbackfn: (this: This, value: T, value2: T, set: Set<T>) => unknown, thisArg: This): void;
    has(value: T): boolean;
    size: number;
}

interface SetConstructor {
    new <T>(iterable?: ?Iterable<T>): Set<T>;
    readonly prototype: Set<any>;
}
declare var Set: SetConstructor;

interface ReadonlySet<out T> {
    forEach<This>(callbackfn: (this: This, value: T, value2: T, set: ReadonlySet<T>) => unknown, thisArg: This): void;
    has(
      // $FlowFixMe[incompatible-variance]
      value: T
    ): boolean;
    size: number;
}

type $ReadOnlySet<out T> = ReadonlySet<T>;

interface WeakSet<T extends WeakKey> extends ReadonlyWeakSet<T> {
    add(value: T): this;
    delete(value: T): boolean;
    has(value: T): boolean;
}

interface WeakSetConstructor {
    new <T extends WeakKey>(iterable?: ?Iterable<T>): WeakSet<T>;
    readonly prototype: WeakSet<WeakKey>;
}
declare var WeakSet: WeakSetConstructor;
