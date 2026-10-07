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
interface ReadonlyArray<out T> {
    /**
     * Returns the value of the last element in the array where predicate is true, and undefined
     * otherwise.
     * @param callbackfn find calls predicate once for each element of the array, in reverse
     * order, until it finds one where predicate returns true. If such an element is found, find
     * immediately returns that element value. Otherwise, find returns undefined.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    findLast<This>(callbackfn: (this : This, value: T, index: number, array: ReadonlyArray<T>) => unknown, thisArg: This): T | void;
    /**
     * Returns the index of the last element in the array where predicate is true, and -1
     * otherwise.
     * @param callbackfn find calls predicate once for each element of the array, in reverse
     * order, until it finds one where predicate returns true. If such an element is found,
     * findLastIndex immediately returns that element index. Otherwise, findLastIndex returns -1.
     * @param thisArg If provided, it will be used as the this value for each invocation of
     * predicate. If it is not provided, undefined is used instead.
     */
    findLastIndex<This>(callbackfn: (this : This, value: T, index: number, array: ReadonlyArray<T>) => unknown, thisArg: This): number;
    /**
     * Returns a new array with the elements in reversed order.
     * It is the copying counterpart of the reverse() method.
     */
    toReversed(
      // $FlowFixMe[incompatible-variance]
    ): Array<T>;
    /**
     * Returns a new array with the elements sorted in ascending order.
     * It is the copying counterpart of the sort() method.
     * @param compareFn Specifies a function that defines the sort order.
     * If omitted, the array elements are converted to strings, then sorted according
     * to each character's Unicode code point value.
     */
    toSorted(
      compareFn?: (a: T, b: T) => number
      // $FlowFixMe[incompatible-variance]
    ): Array<T>;
    /**
     * Returns a new array with some elements removed and/or replaced at a given index.
     * It is the copying counterpart of the splice() method.
     * @param start Zero-based index at which to start changing the array, converted to an integer.
     * @param deleteCount An integer indicating the number of elements in the array to remove from start.
     * @param items The elements to add to the array, beginning from start.
     */
    toSpliced<S>(
      start: number, deleteCount?: number, ...items: Array<S>
      // $FlowFixMe[incompatible-variance]
    ): Array<T | S>;
    /**
     * Returns a new array with the element at the given index replaced with the given value.
     * It is the copying version of using the bracket notation to change the value of a given index.
     * @param index Zero-based index at which to change the array, converted to an integer.
     * @param value Any value to be assigned to the given index.
     */
    with(
      // $FlowFixMe[incompatible-variance]
      index: number, value: T
      // $FlowFixMe[incompatible-variance]
    ): Array<T>;
}
