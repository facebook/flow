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
interface $IteratorProtocol<out Yield,out Return=void,in Next=void> {
    next(value?: Next): IteratorResult<Yield,Return>;
}

/**
 * The built-in Iterator abstract base class. Iterators for Arrays, Strings, and Generators all inherit from this class.
 * Extend this class to implement custom iterators that support all iterator helper methods.
 * Note that you can use `Iterator.from()` to get an iterator helper wrapping any object that implements
 * the iterator protocol.
 */
declare class Iterator<out Yield,out Return=void,in Next=void> implements $IteratorProtocol<Yield,Return,Next>, $Iterable<Yield,Return,Next> {
    @@iterator(): Iterator<Yield,Return,Next>;
    next(value?: Next): IteratorResult<Yield,Return>;
    /**
     * Returns a new iterator that yields that values returned by calling the argument callback on each value yielded by this iterator.
     * @param callbackfn A function that accepts up to two arguments: the yielded value and its index. The map method calls the callbackfn function once per iteration.
     */
    map<U>(callbackfn: (value: Yield, index: number) => U): Iterator<U, void, unknown>;
    /**
     * Returns a new iterator that yields only those elements of the iterator for which the provided predicate returns a truthy value.
     * @param callbackfn A function that accepts up to two arguments: the yielded value and its index. The filter method calls the predicate function once per iteration.
     */
    filter(callbackfn: typeof Boolean): Iterator<NonNullable<Yield>, void, unknown>;
    /**
     * Returns a new iterator that yields only those elements of the iterator for which the provided predicate returns a truthy value.
     * @param callbackfn A function that accepts up to two arguments: the yielded value and its index. The filter method calls the predicate function once per iteration.
     */
    filter<
      // $FlowFixMe[incompatible-variance]
      Refined extends Yield
    >(callbackfn: (value: Yield, index: number) => implies value is Refined): Iterator<Refined, void, unknown>;
    /**
     * Returns a new iterator that yields only those elements of the iterator for which the provided predicate returns a truthy value.
     * @param callbackfn A function that accepts up to two arguments: the yielded value and its index. The filter method calls the predicate function once per iteration.
     */
    filter(callbackfn: (value: Yield, index: number) => unknown): Iterator<Yield, void, unknown>;
    /**
     * Returns a new iterator that yields up to the given number of elements in this iterator and then terminates.
     * @param limit The maximum number of values to yield.
     */
    take(limit: number): Iterator<Yield, void, unknown>;
    /**
     * Returns a new iterator that yields values from this iterator after skipping the provided count. If the this iterator has fewer than limit elements, the new iterator will be completed the first time next() is called.
     * @param count The number of values to drop.
     */
    drop(count: number): Iterator<Yield, void, unknown>;
    /**
     * Returns a new iterator that takes each value yielded by this iterator, runs it through the argument mapping function to get another iterable, and yields each value returned by these mapped iterables.
     * @param callbackfn A function that accepts up to two arguments: the yielded value and its index. The map method calls the callbackfn function once per iteration. The callback must return an iterable.
     */
    flatMap<U>(callbackfn: (value: Yield, index: number) => $IteratorProtocol<U, unknown, void> | $Iterable<U, unknown, void>): Iterator<U, void, unknown>;
    /**
     * Calls the argument "reducer" callback function on each value produced by this iterator, passing in the return value from the calculation on the preceding iteration. The final result of running the reducer across all elements is a single value.
     * Exhausts this iterator. Will hang if it's infinite.
     * @param callbackfn A function that accepts up to three arguments: the accumulated value, the current value, and the current index.
     * @param initialValue If initialValue is specified, it is used as the initial value to start
     * the accumulation. If omitted, the first call of the callback gets the first two iterated values.
     */
    reduce<U>(
      callbackfn: (previousValue: Yield | U, currentValue: Yield, index: number) => U,
    ): Yield | U;
    /**
     * Calls the argument "reducer" callback function on each value produced by this iterator, passing in the return value from the calculation on the preceding iteration. The final result of running the reducer across all elements is a single value.
     * Exhausts this iterator. Will hang if it's infinite.
     * @param callbackfn A function that accepts up to three arguments: the accumulated value, the current value, and the current index.
     * @param initialValue If initialValue is specified, it is used as the initial value to start
     * the accumulation. If omitted, the first call of the callback gets the first two iterated values.
     */
    reduce<U>(
      callbackfn: (previousValue: U, currentValue: Yield, index: number) => U,
      initialValue: U
    ): U;
    /**
     * Creates a new array from the values yielded by this iterator.
     * Exhausts this iterator. Will hang if it's infinite.
     */
    toArray(
      // $FlowFixMe[incompatible-variance]
    ): Array<Yield>;
    /**
     * Calls the argument function for each value yielded by the iterator. Any returned values are ignored.
     * Exhausts this iterator. Will hang if it's infinite.
     * @param callbackfn  A function that accepts up to two arguments: the yielded value and its index.
     */
    forEach(callbackfn: (value: Yield, index: number) => unknown): void;
    /**
     * Calls the argument function for each value yielded by the iterator.
     * Returns true if the predicate function ever returns a truthy value.
     * Exhausts this iterator. Will hang if it's infinite.
     * @param callbackfn  A function that accepts up to two arguments: the yielded value and its index.
     */
    some(callbackfn: (value: Yield, index: number) => unknown): boolean;
    /**
     * Calls the argument function for each value yielded by the iterator.
     * Returns true if the predicate function returns a truthy value for all iterated values.
     * Exhausts this iterator. Will hang if it's infinite.
     * @param callbackfn  A function that accepts up to two arguments: the yielded value and its index.
     */
    every(callbackfn: (value: Yield, index: number) => unknown): boolean;
    /**
     * Calls the argument function for values yielded by the iterator until one returns a truthy value.
     * Returns the first value that produced a truthy predicate.
     * Returns undefined if the iterator completes without a match.
     * Iterates this iterator. Will hang if it's infinite and has no matches.
     * @param callbackfn  A function that accepts up to two arguments: the yielded value and its index.
     */
    find<
      // $FlowFixMe[incompatible-variance]
      Refined extends Yield
    >(predicate: (value: Yield, index: number) => implies value is Refined): Refined | void;
    find(callbackfn: (value: Yield, index: number) => unknown): Yield | void;
    /**
     * This method allows wrapping objects that implement the iterator protocol in an Iterator to get the built-in helpers.
     * @returns The argument object if it's already an Iterator subclass, or a wrapping Iterator if the argument objects implements the iterable or iterator protocols.
     */
    static from<Yield, Return, Next>(
      source: $IteratorProtocol<Yield,Return,Next> | $Iterable<Yield,Return,Next>
    ): Iterator<Yield,Return,Next>;
}

type $Iterator<out Yield,out Return,in Next> = Iterator<Yield,Return,Next>;

/**
 * The iterable protocol expected by built-ins like Array.from().
 * You can implement this yourself.
 */
interface $Iterable<out Yield,out Return,in Next> {
    @@iterator(): $IteratorProtocol<Yield,Return,Next>;
}
type Iterable<out T> = $Iterable<T,void,void>;

type IterableIterator<out T> = Iterator<T>;
type ArrayIterator<out T> = Iterator<T>;
type StringIterator = Iterator<string>;
type IteratorObject<out T, TReturn = void, TNext = void> = Iterator<T, TReturn, TNext>;
type BuiltinIteratorReturn = void;
type MapIterator<out T> = Iterator<T>;
type SetIterator<out T> = Iterator<T>;
type IteratorReturnResult<out TReturn> = {
  done: true,
  readonly value?: TReturn,
  ...
};
