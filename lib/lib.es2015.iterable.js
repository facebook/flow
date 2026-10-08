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
/* Iterable/Iterator/Generator */

type IteratorResult<out Yield,out Return> =
  | {
    done: true,
    readonly value?: Return,
    ...
}
  | {
    done: false,
    readonly value: Yield,
    ...
  };

/**
 * The iterator protocol expected by built-ins like Iterator.from().
 * You can implement this yourself.
 */
interface Iterator<out Yield,out Return=void,in Next=void> {
    next(value?: Next): IteratorResult<Yield,Return>;
}

/**
 * The built-in Iterator abstract base class. Iterators for Arrays, Strings, and Generators all inherit from this class.
 * Extend this class to implement custom iterators that support all iterator helper methods.
 * Note that you can use `Iterator.from()` to get an iterator helper wrapping any object that implements
 * the iterator protocol.
 */
interface IteratorObject<out Yield,out Return=void,in Next=void> extends Iterator<Yield,Return,Next>, Iterable<Yield,Return,Next> {
    @@iterator(): IteratorObject<Yield,Return,Next>;
    next(value?: Next): IteratorResult<Yield,Return>;
}

type $IteratorProtocol<out Yield,out Return=void,in Next=void> = Iterator<Yield,Return,Next>;
type $Iterator<out Yield,out Return,in Next> = IteratorObject<Yield,Return,Next>;

/**
 * The iterable protocol expected by built-ins like Array.from().
 * You can implement this yourself.
 */
interface Iterable<out Yield,out Return=void,in Next=void> {
    @@iterator(): Iterator<Yield,Return,Next>;
}
type $Iterable<out Yield,out Return,in Next> = Iterable<Yield,Return,Next>;

type IterableIterator<out T> = IteratorObject<T>;
type ArrayIterator<out T> = IteratorObject<T>;
type StringIterator = IteratorObject<string>;
type BuiltinIteratorReturn = void;
type MapIterator<out T> = IteratorObject<T>;
type SetIterator<out T> = IteratorObject<T>;
type IteratorReturnResult<out TReturn> = {
  done: true,
  readonly value?: TReturn,
  ...
};

interface SymbolConstructor {
  /**
   * A method that returns the default iterator for an object. Called by the semantics of the
   * for-of statement.
   */
  readonly iterator: '@@iterator'; // polyfill '@@iterator'
}

interface ReadonlyArray<out T> {
    @@iterator(): ArrayIterator<T>;
    /**
     * Returns an iterable of key, value pairs for every entry in the array
     */
    // $FlowFixMe[incompatible-variance]
    entries(): ArrayIterator<[number, T]>;
    /**
     * Returns an iterable of keys in the array
     */
    keys(): ArrayIterator<number>;
    /**
     * Returns an iterable of values in the array
     */
    values(): ArrayIterator<T>;
}

interface Map<K, V> {
    @@iterator(): MapIterator<[K, V]>;
    /**
     * Returns an iterable of key, value pairs for every entry in the map.
     */
    entries(): MapIterator<[K, V]>;
    /**
     * Returns an iterable of keys in the map
     */
    keys(): MapIterator<K>;
    /**
     * Returns an iterable of values in the map
     */
    values(): MapIterator<V>;
}

interface ReadonlyMap<K, out V> {
    @@iterator(
      // $FlowFixMe[incompatible-variance]
    ): MapIterator<[K, V]>;
    /**
     * Returns an iterable of key, value pairs for every entry in the map.
     */
    entries(
      // $FlowFixMe[incompatible-variance]
    ): MapIterator<[K, V]>;
    /**
     * Returns an iterable of keys in the map
     */
    keys(): MapIterator<K>;
    /**
     * Returns an iterable of values in the map
     */
    values(): MapIterator<V>;
}

interface Set<T> {
    @@iterator(): SetIterator<T>;
    /**
     * Returns an iterable of [v,v] pairs for every value `v` in the set.
     */
    entries(): SetIterator<[T, T]>;
    /**
     * Despite its name, returns an iterable of the values in the set,
     */
    keys(): SetIterator<T>;
    /**
     * Returns an iterable of values in the set.
     */
    values(): SetIterator<T>;
}

interface ReadonlySet<out T> {
    @@iterator(): SetIterator<T>;
    /**
     * Returns an iterable of [v,v] pairs for every value `v` in the set.
     */
    entries(
      // $FlowFixMe[incompatible-variance]
    ): SetIterator<[T, T]>;
    /**
     * Despite its name, returns an iterable of the values in the set,
     */
    keys(): SetIterator<T>;
    /**
     * Returns an iterable of values in the set.
     */
    values(): SetIterator<T>;
}

interface String {
    @@iterator(): StringIterator;
}

interface $TypedArrayInternal<
  T extends number | bigint,
  out TArrayBuffer extends ArrayBufferLike = ArrayBufferLike,
  out TArrayResult = $TypedArray<TArrayBuffer>,
> {
    @@iterator(): ArrayIterator<T>;
    /**
     * Returns an array of key, value pairs for every entry in the array
     */
    entries(): ArrayIterator<[number, T]>;
    /**
     * Returns an list of keys in the array
     */
    keys(): ArrayIterator<number>;
    /**
     * Returns an list of values in the array
     */
    values(): ArrayIterator<T>;
}
